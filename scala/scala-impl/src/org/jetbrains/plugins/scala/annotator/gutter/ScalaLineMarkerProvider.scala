package org.jetbrains.plugins.scala
package annotator
package gutter

import java.util

import com.intellij.codeHighlighting.Pass
import com.intellij.codeInsight.daemon._
import com.intellij.icons.AllIcons
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.colors.{CodeInsightColors, EditorColorsManager}
import com.intellij.openapi.editor.markup.{GutterIconRenderer, SeparatorPlacement}
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi._
import com.intellij.psi.search.searches.ClassInheritorsSearch
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.{FunctionUtil, Function => IJFunction}
import javax.swing.Icon
import org.jetbrains.plugins.scala.annotator.gutter.GutterIcons._
import org.jetbrains.plugins.scala.annotator.gutter.GutterUtil._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScFieldId, ScReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScCaseClauses, ScPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.search.ScalaOverridingMemberSearcher
import org.jetbrains.plugins.scala.lang.psi.types.Signature
import org.jetbrains.plugins.scala.util.SAMUtil._

import scala.collection.Seq

/**
 * User: Alexander Podkhalyuzin
 * Date: 31.10.2008
 */
class ScalaLineMarkerProvider(daemonSettings: DaemonCodeAnalyzerSettings, colorsManager: EditorColorsManager)
    extends LineMarkerProvider
    with ScalaSeparatorProvider {

  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[_ <: PsiElement] =
    if (element.isValid) {
      val lineMarkerInfo =
        getOverridesImplementsMarkers(element)
          .orElse(getImplementsSAMTypeMarker(element))
          .orNull

      if (daemonSettings.SHOW_METHOD_SEPARATORS && isSeparatorNeeded(element)) {
        if (lineMarkerInfo == null) {
          addSeparatorInfo(createMarkerInfo(element))
        } else {
          addSeparatorInfo(lineMarkerInfo)
        }
      } else lineMarkerInfo
    } else null

  private[this] def createMarkerInfo(element: PsiElement): LineMarkerInfo[PsiElement] = {
    val leaf = PsiTreeUtil.firstChild(element).toOption.getOrElse(element)
    new LineMarkerInfo[PsiElement](
      leaf,
      leaf.getTextRange,
      null,
      Pass.UPDATE_ALL,
      FunctionUtil.nullConstant[PsiElement, String](),
      null,
      GutterIconRenderer.Alignment.RIGHT
    )
  }

  private[this] def addSeparatorInfo(info: LineMarkerInfo[_ <: PsiElement]): LineMarkerInfo[_ <: PsiElement] = {
    info.separatorColor     = colorsManager.getGlobalScheme.getColor(CodeInsightColors.METHOD_SEPARATORS_COLOR)
    info.separatorPlacement = SeparatorPlacement.TOP
    info
  }

  private[this] def arrowUpLineMarker(
    element:            PsiElement,
    icon:               Icon,
    markerType:         ScalaMarkerType,
    presentationParent: Option[PsiElement] = None
  ): LineMarkerInfo[PsiElement] =
    ArrowUpOrDownLineMarkerInfo(
      element,
      icon,
      markerType,
      Pass.UPDATE_ALL,
      GutterIconRenderer.Alignment.LEFT,
      presentationParent
    )

  /* Validates that this psi element can be the first one in a lambda */
  private[this] def canBeFunctionalExpressionAnchor(e: PsiElement): Boolean =
    e.getNode.getElementType match {
      case ScalaTokenTypes.tLBRACE => e.getNextSiblingNotWhitespace.isInstanceOf[ScCaseClauses]
      case ScalaTokenTypes.tIDENTIFIER | ScalaTokenTypes.tUNDER | ScalaTokenTypes.tLPARENTHESIS =>
        true
      case _ => false
    }

  private[this] def funExprParent(element: PsiElement): Option[(ScExpression, PsiClass)] =
    element.parentsInFile.collectFirst {
      case _: ScMember            => None
      case e @ SAMTypeParent(sam) => Option(e -> sam)
    }.flatten

  private[this] val trivialSAMs: Set[String] = Set("scala.Function", "scala.PartialFunction", "java.util.function")
  private[this] def isInterestingSAM(sam: PsiClass): Boolean = !trivialSAMs.exists(sam.qualifiedName.startsWith)

  private[this] def getImplementsSAMTypeMarker(element: PsiElement): Option[LineMarkerInfo[_ <: PsiElement]] =
    if (canBeFunctionalExpressionAnchor(element)) for {
      (parent, sam) <- funExprParent(element)
      if isInterestingSAM(sam) &&
        PsiTreeUtil.getDeepestFirst(parent) == element
      icon       = AllIcons.Gutter.ImplementingFunctionalInterface
      markerType = ScalaMarkerType.samTypeImplementation(sam)
    } yield arrowUpLineMarker(element, icon, markerType, Option(parent))
    else None

  private[this] def getOverridesImplementsMarkers(element: PsiElement): Option[LineMarkerInfo[_ <: PsiElement]] = {
    val isIdentifier = element.getNode.getElementType == ScalaTokenTypes.tIDENTIFIER
    val notReference = element.parent.exists {
      case _: ScReferenceElement => false
      case  _                    => true
    }

    if (isIdentifier && notReference) {
      def containsNamedElement(holder: ScDeclaredElementsHolder) =
        holder.declaredElements.exists(_.asInstanceOf[ScNamedElement].nameId == element)

      val text = element.getText

      namedParent(element).flatMap {
        case method: ScFunction if method.isInstance && method.name == text =>
          val signatures = method.superSignaturesIncludingSelfType
          val icon       = getOverridesOrImplementsIcon(method, signatures)
          val markerType = ScalaMarkerType.overridingMember
          if (signatures.nonEmpty) arrowUpLineMarker(element, icon, markerType).toOption
          else None
        case cParam: ScClassParameter if cParam.name == text =>
          val signatures = ScalaPsiUtil.superValsSignatures(cParam, withSelfType = true)
          val icon       = getOverridesOrImplementsIcon(cParam, signatures)
          val markerType = ScalaMarkerType.overridingMember
          if (signatures.nonEmpty) arrowUpLineMarker(element, icon, markerType).toOption
          else None
        case v: ScValueOrVariable if v.isInstance && containsNamedElement(v) =>
          val bindings   = v.declaredElements.filter(_.name == element.getText)
          val signatures = bindings.flatMap(ScalaPsiUtil.superValsSignatures(_, withSelfType = true))
          val icon       = getOverridesOrImplementsIcon(v, signatures)
          val markerType = ScalaMarkerType.overridingMember
          if (signatures.nonEmpty) arrowUpLineMarker(element, icon, markerType).toOption
          else None
        case ta: ScTypeAlias if ta.isInstance && ta.name == text =>
          val elements = ScalaPsiUtil.superTypeMembers(ta, withSelfType = true)
          val icon     = IMPLEMENTING_METHOD_ICON
          val typez    = ScalaMarkerType.overridingMember
          if (elements.nonEmpty) arrowUpLineMarker(element, icon, typez).toOption
          else None
        case _ => None
      }
    } else None
  }

  override def collectSlowLineMarkers(
    elements: util.List[PsiElement],
    result:   util.Collection[LineMarkerInfo[_ <: PsiElement]]
  ): Unit = {
    import scala.collection.JavaConverters._

    ApplicationManager.getApplication.assertReadAccessAllowed()
    elements.asScala.collect {
      case ident if ident.getNode.getElementType == ScalaTokenTypes.tIDENTIFIER => ident
    }.flatMap { identifier =>
      ProgressManager.checkCanceled()
      val context = identifier.parent match {
        case Some(_: ScPattern | _: ScFieldId) => namedParent(identifier)
        case other                             => other
      }

      context match {
        case Some(tDef: ScTypeDefinition)                => collectInheritingClassesMarker(tDef)
        case Some(member: ScMember) if member.isInstance => collectOverriddenMemberMarker(member, identifier)
        case _                                           => None
      }
    }.foreach(result.add)
  }
}

private object GutterUtil {
  private[gutter] final case class ArrowUpOrDownLineMarkerInfo(
    element:            PsiElement,
    icon:               Icon,
    markerType:         ScalaMarkerType,
    passId:             Int,
    alignment:          GutterIconRenderer.Alignment,
    presentationParent: Option[PsiElement] = None
  ) extends MergeableLineMarkerInfo[PsiElement](
        element,
        element.getTextRange,
        icon,
        passId,
        markerType.tooltipProvider,
        markerType.navigationHandler,
        alignment
      ) {
    override def canMergeWith(other: MergeableLineMarkerInfo[_]): Boolean = other match {
      case that: ArrowUpOrDownLineMarkerInfo => icon == that.icon
      case _                                 => false
    }

    override def getCommonIcon(list: util.List[MergeableLineMarkerInfo[_ <: PsiElement]]): Icon = icon

    override def getCommonTooltip(
      infos: util.List[MergeableLineMarkerInfo[_ <: PsiElement]]
    ): IJFunction[_ >: PsiElement, String] =
      _ =>
        markerType match {
          case ScalaMarkerType.overriddenMember => ScalaBundle.message("multiple.overriden.tooltip")
          case _                                => ScalaBundle.message("multiple.overriding.tooltip")
      }

    override def getElementPresentation(element: PsiElement): String =
      presentationParent.fold(super.getElementPresentation(element))(
        parent => StringUtil.shortenTextWithEllipsis(parent.getText, 100, 0)
      )
  }

  def getOverridesOrImplementsIcon(e: PsiElement, signatures: Seq[Signature]): Icon =
    if (isOverrides(e, signatures)) OVERRIDING_METHOD_ICON
    else IMPLEMENTING_METHOD_ICON

  def namedParent(e: PsiElement): Option[PsiElement] =
    e.withParentsInFile.find(ScalaPsiUtil.isNameContext)

  def collectInheritingClassesMarker(aClass: ScTypeDefinition): Option[LineMarkerInfo[_ <: PsiElement]] = {
    val inheritor = ClassInheritorsSearch.search(aClass, false).findFirst.toOption
    inheritor.map { _ =>
      val range = aClass.nameId.getTextRange

      val icon = aClass match {
        case _: ScTrait => IMPLEMENTED_INTERFACE_MARKER_RENDERER
        case _          => SUBCLASSED_CLASS_MARKER_RENDERER
      }

      val markerType = ScalaMarkerType.subclassedClass
      new LineMarkerInfo[PsiElement](
        aClass.nameId,
        range,
        icon,
        Pass.LINE_MARKERS,
        markerType.tooltipProvider,
        markerType.navigationHandler,
        GutterIconRenderer.Alignment.RIGHT
      )
    }
  }

  def collectOverriddenMemberMarker(member: ScMember, anchor: PsiElement): Option[LineMarkerInfo[_ <: PsiElement]] =
    member match {
      case method: PsiMethod if method.isConstructor => None
      case _ =>
        val namedElems: Seq[ScNamedElement] = member match {
          case d: ScDeclaredElementsHolder => d.declaredElements.filterBy[ScNamedElement]
          case param: ScClassParameter     => Seq(param)
          case ta: ScTypeAlias             => Seq(ta)
          case _                           => Seq.empty
        }

        val overrides = namedElems.flatMap(ScalaOverridingMemberSearcher.search(_, deep = false, withSelfType = true))

        overrides.nonEmpty.option {
          val icon =
            if (!isAbstract(member)) OVERRIDDEN_METHOD_MARKER_RENDERER
            else IMPLEMENTED_INTERFACE_MARKER_RENDERER

          val markerType = ScalaMarkerType.overriddenMember
          ArrowUpOrDownLineMarkerInfo(
            anchor,
            icon,
            markerType,
            Pass.LINE_MARKERS,
            GutterIconRenderer.Alignment.RIGHT
          )
        }
    }

  def isOverrides(element: PsiElement, supers: Seq[Signature]): Boolean =
    element match {
      case _: ScFunctionDeclaration  => true
      case _: ScValueDeclaration     => true
      case _: ScVariableDeclaration  => true
      case _: ScTypeAliasDeclaration => true
      case _ =>
        val iter = supers.iterator
        while (iter.hasNext) {
          val s = iter.next()
          ScalaPsiUtil.nameContext(s.namedElement) match {
            case _: ScFunctionDefinition                          => return true
            case _: ScFunction                                    =>
            case method: PsiMethod if !method.hasAbstractModifier => return true
            case _: ScVariableDefinition | _: ScPatternDefinition => return true
            case f: PsiField if !f.hasAbstractModifier            => return true
            case _: ScVariableDeclaration                         =>
            case _: ScValueDeclaration                            =>
            case _: ScParameter                                   => return true
            case _: ScTypeAliasDefinition                         => return true
            case _: ScTypeAliasDeclaration                        =>
            case _: PsiClass                                      => return true
            case _                                                =>
          }
        }
        false
    }

  def isAbstract(element: PsiElement): Boolean = element match {
    case _: ScFunctionDeclaration  => true
    case _: ScValueDeclaration     => true
    case _: ScVariableDeclaration  => true
    case _: ScTypeAliasDeclaration => true
    case _                         => false
  }
}
