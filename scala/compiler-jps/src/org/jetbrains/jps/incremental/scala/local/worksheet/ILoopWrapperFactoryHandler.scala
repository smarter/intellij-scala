package org.jetbrains.jps.incremental.scala.local.worksheet

import java.io.{File, OutputStream}
import java.net.{URLClassLoader, URLDecoder}
import java.util

import com.intellij.openapi.util.io.FileUtil
import com.martiansoftware.nailgun.ThreadLocalPrintStream
import org.jetbrains.jps.incremental.scala.Client
import org.jetbrains.jps.incremental.scala.data.{CompilerJars, SbtData}
import org.jetbrains.jps.incremental.scala.local.{CompilerFactoryImpl, NullLogger}
import org.jetbrains.jps.incremental.scala.remote.{Arguments, WorksheetOutputEvent}
import sbt.internal.inc.classpath.ClasspathUtilities
import sbt.internal.inc.{AnalyzingCompiler, RawCompiler}
import sbt.io.Path
import xsbti.compile.{ClasspathOptionsUtil, ScalaInstance}

class ILoopWrapperFactoryHandler {
  import ILoopWrapperFactoryHandler._
  
  private var replFactory: Option[(Class[_], Any, String)] = None

  def loadReplWrapperAndRun(commonArguments: Arguments, out: OutputStream, client: Option[Client]) {
    val compilerJars = commonArguments.compilerData.compilerJars.orNull
    val scalaInstance = CompilerFactoryImpl.createScalaInstance(compilerJars)
    val scalaVersion = findScalaVersionIn(scalaInstance)
    val iLoopFile = getOrCompileReplLoopFile(commonArguments.sbtData, scalaInstance, client)
    
    replFactory match {
      case Some((_, _, oldVersion)) if oldVersion == scalaVersion =>
      case _ =>
        val loader = createIsolatingClassLoader(compilerJars)
        val clazz = loader.loadClass(REPL_FQN)
        val javaILoopWrapper  = clazz.newInstance()
        replFactory = Option((clazz, javaILoopWrapper, scalaVersion))
    }

    client.foreach(_ progress "Running REPL...")
    
    replFactory foreach {
      case (clazz, instance, _) =>
        WorksheetServer.patchSystemOut(out)

        val parameterTypes = Array(
          classOf[java.util.List[String]],
          classOf[String],
          classOf[File],
          classOf[File],
          classOf[java.util.List[File]],
          classOf[java.util.List[File]],
          classOf[java.io.OutputStream],
          classOf[java.io.File],
          classOf[Comparable[String]]
        )
        val loadReplWrapperAndRunMethod = clazz.getDeclaredMethod("loadReplWrapperAndRun", parameterTypes: _*)

        withFilteredPath {
          val clientProvider: Object = if (client.isEmpty) null else new Comparable[String] {
            override def compareTo(o: String): Int = {
              client.get.progress(o)
              0
            }
          }
          val args: Array[Object] = Array(
            scalaToJava(commonArguments.worksheetFiles),
            commonArguments.compilationData.sources.headOption.map(_.getName).getOrElse(""),
            compilerJars.library,
            compilerJars.compiler,
            scalaToJava(compilerJars.extra),
            scalaToJava(commonArguments.compilationData.classpath),
            out,
            iLoopFile,
            clientProvider
          )
          loadReplWrapperAndRunMethod.invoke(instance, args: _*)
        }
    }
  }
  
  protected def getOrCompileReplLoopFile(sbtData: SbtData, scalaInstance: ScalaInstance, client: Option[Client]): File = {
    val home = sbtData.interfacesHome
    val interfaceJar = sbtData.compilerInterfaceJar

    val sourceJar = {
      val f = sbtData.sourceJars._2_11
      new File(f.getParent, "repl-interface-sources.jar")
    }

    val version = findScalaVersionIn(scalaInstance)
    val is213 = version.startsWith("2.13")
    val replLabel = s"repl-wrapper-$version-${sbtData.javaClassVersion}-$WRAPPER_VERSION-${
      if (is213) "ILoopWrapper213Impl" else "ILoopWrapperImpl"}.jar"
    val targetFile = new File(home, replLabel)

    if (!targetFile.exists()) {
      val log = NullLogger
      home.mkdirs()

      findContainingJar(this.getClass) foreach {
        thisJar =>
          client.foreach(_.progress("Compiling REPL runner..."))

          val filter = (file: File) => is213 ^ !file.getName.endsWith("213Impl.scala")

          AnalyzingCompiler.compileSources(
            Seq(sourceJar), targetFile, Seq(interfaceJar, thisJar), replLabel,
            new RawCompiler(scalaInstance, ClasspathOptionsUtil.auto(), log) {
              override def apply(sources: Seq[File], classpath: Seq[File], outputDirectory: File, options: Seq[String]): Unit = {
                super.apply(sources.filter(filter), classpath, outputDirectory, options)
              }
            }, log
          )
      }
    }


    targetFile
  }
}

object ILoopWrapperFactoryHandler {
  private val WRAPPER_VERSION = 1
  private val REPL_FQN = "org.jetbrains.jps.incremental.scala.local.worksheet.compatibility.JavaILoopWrapperFactory"

  private val JAVA_USER_CP_KEY = "java.class.path"
  private val STOP_WORDS = Set("scala-library.jar", "scala-nailgun-runner.jar", "nailgun.jar", "compiler-shared.jar",
    "incremental-compiler.jar", "compiler-jps.jar", "hydra-compiler-jps.jar")


  private def withFilteredPath(action: => Unit) {
    val oldCp = System.getProperty(JAVA_USER_CP_KEY)

    if (oldCp == null) {
      action
      return
    }

    val newCp = oldCp.split(File.pathSeparatorChar).map(
      new File(_).getAbsoluteFile
    ).filter {
      file => file.exists() && !STOP_WORDS.contains(file.getName)
    }.map(_.getAbsolutePath).mkString(File.pathSeparator)

    System.setProperty(JAVA_USER_CP_KEY, newCp)
    
    try {
      action
    } finally {
      System.setProperty(JAVA_USER_CP_KEY, oldCp)
    }
  }
  
  private def findScalaVersionIn(scalaInstance: ScalaInstance): String = 
    CompilerFactoryImpl.readScalaVersionIn(scalaInstance.loader).getOrElse("Undefined")

  private def findContainingJar(clazz: Class[_]): Option[File] = {
    val resource = clazz.getResource(s"/${clazz.getName.replace('.', '/')}.class")

    if (resource == null) return None

    val url = URLDecoder.decode(resource.toString.stripPrefix("jar:file:"), "UTF-8")
    val idx = url.indexOf(".jar!")
    if (idx == -1) return None

    Some(new File(url.substring(0, idx + 4))).filter(_.exists())
  }

  private def findContainingJars(classes: Seq[Class[_]]): Seq[File] =
    classes.flatMap(findContainingJar)

  private def getBaseJars(compiler: CompilerJars): Seq[File] = {
    val compilerJars = compiler.library +: compiler.compiler +: compiler.extra
    val additionalJars = findContainingJars(Seq(
      this.getClass,
      classOf[FileUtil],
      classOf[ThreadLocalPrintStream],
      classOf[WorksheetOutputEvent]
    ))
    compilerJars ++ additionalJars
  }

  private def createIsolatingClassLoader(compilerJars: CompilerJars): URLClassLoader = {
    new URLClassLoader(Path.toURLs(getBaseJars(compilerJars)), ClasspathUtilities.rootLoader)
  }

  //We need this method as scala std lib converts scala collections to its own wrappers with asJava method
  private def scalaToJava[T](seq: Seq[T]): util.List[T] = {
    val al = new util.ArrayList[T]()
    seq.foreach(al.add)
    al
  }
}