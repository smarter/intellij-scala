package org.jetbrains.bsp.protocol

import java.io.File
import java.net.{ServerSocket, URI}
import java.nio.file.Files

import io.circe._, io.circe.parser.decode, io.circe.generic.semiauto._
import com.intellij.openapi.diagnostic.Logger
import ch.epfl.scala.bsp.InitializeBuildParams
import monix.eval.Task
import org.jetbrains.bsp.{BspError, BspErrorMessage, BspException}
import org.jetbrains.bsp.BspUtil.IdeaLoggerOps
import org.jetbrains.bsp.protocol.BspServerConnector._

import scala.meta.jsonrpc._
import scala.sys.process.{Process, ProcessLogger}

abstract class BspServerConnector(initParams: InitializeBuildParams) {
  /**
    * Connect to a bsp server with one of the given methods.
    * @param methods methods supported by the bsp server, in order of preference
    * @return None if no compatible method is found. TODO should be an error response
    */
  def connect(methods: BspConnectionMethod*): Task[Either[BspError, BspSession]]
}

object BspServerConnector {
  sealed abstract class BspConnectionMethod
  final case class UnixLocalBsp(socketFile: File) extends BspConnectionMethod
  final case class WindowsLocalBsp(pipeName: String) extends BspConnectionMethod
  final case class TcpBsp(host: URI, port: Int) extends BspConnectionMethod
}

case class BspConfig(launcherCommand: String)
object BspConfig {
  implicit val bspDecoder: Decoder[BspConfig] = deriveDecoder[BspConfig]
  implicit val bspEncoder: Encoder[BspConfig] = deriveEncoder[BspConfig]
}

/** TODO Connects to a bsp server based on information in .bsp directory */
class GenericConnector(base: File, initParams: InitializeBuildParams) extends BspServerConnector(initParams) {

  private val logger: Logger = Logger.getInstance(classOf[GenericConnector])

  val bspLauncherConfigFile = new File(base, "bsp-config.json").getCanonicalFile
  val content = new String(Files.readAllBytes(bspLauncherConfigFile.toPath))
  val bspConfig = decode[BspConfig](content).right.get

  private val proclog = ProcessLogger(
    out => logger.info(s"bsp server stdout: $out"),
    err => logger.info(s"bsp server stderr: $err")
  )

  override def connect(methods: BspConnectionMethod*): Task[Either[BspError, BspSession]] = {
    methods.collectFirst {
      case TcpBsp(_, port) =>
        Task {
          val ss = new ServerSocket(5001  /*0*/)
          val port = ss.getLocalPort
          val cmd = s"${bspConfig.launcherCommand} --protocol tcp --port $port"
          Process(cmd, base).run(proclog)
          val socket = ss.accept()
          val cleanup = Task.eval { ss.close() }
          val sLogger = logger.toScribeLogger
          val client = new LanguageClient(socket.getOutputStream, sLogger)
          val messages = BaseProtocolMessage.fromInputStream(socket.getInputStream, sLogger)
          Right(new BspSession(messages, client, initParams, cleanup))
        }
    }.get //.toRight(BspErrorMessage("could not find supported connection method for bsp"))
  }
}

