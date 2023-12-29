package controllers

import javax.inject._
import play.api.mvc._
import routing.{DiagramNodeProcessor, PageRoutingAndQuestions}
import xml.parser.DiagramNode

import scala.xml._

@Singleton
class PageController @Inject() (cc: ControllerComponents)
    extends AbstractController(cc) {

  def showPage(id: Int): Action[AnyContent] = Action {

    val findFinalPage: Either[PageControllerError, DiagramNode] =
      PageController.diagramNodes.find(_.id.toInt == id) match {
        case Some(value) => Right(value)
        case None        => Left(IdNotFoundInDiagramNodes(id))
      }

    findIdFromPageData(id)
      .map(pageData => Ok(views.html.showPage(pageData)))
      .orElse(
        getNextPageFromDiagramNode(findFinalPage)
      ) match {
      case Left(value)  => NotFound(s"$value")
      case Right(value) => value
    }

  }

  def processAnswer(pageId: Int): Action[AnyContent] = Action { request =>
    val destination: Either[PageControllerError, Int] = for {
      bodyForm <- request.body.asFormUrlEncoded match {
        case Some(value) => Right(value)
        case None        => Left(AsFormUrlEncoded(pageId))
      }
      answers <- bodyForm.get("answers[]") match {
        case Some(value) => Right(value)
        case None        => Left(GetAnswersError(pageId))
      }
      pageRoutingAndQuestions <- findIdFromPageData(pageId)
      matchAnswer <- answers match {
        case singleAnswer :: Nil => Right(singleAnswer)
        case _                   => Left(MoreThanOneAnswersError(pageId, answers))
      }
      answerWithRoute <- pageRoutingAndQuestions.potentialAnswers.find(_.ans == matchAnswer) match {
        case Some(value) => Right(value)
        case None        => Left(IdNotFoundInPageRoutingData(pageId))
      }

    } yield answerWithRoute.destination

    val getNextPageWhenSingleAnswer: Either[PageControllerError, Int] = for {
      pageRoutingAndQuestions <- findIdFromPageData(pageId)
      destination <- pageRoutingAndQuestions.potentialAnswers match {
        case singleAnswer :: Nil => Right(singleAnswer.destination)
        case _ =>
          Left(MoreThanOneAnswersError(pageId, pageRoutingAndQuestions.potentialAnswers.map(_.ans)))
      }
    } yield destination

    destination.orElse(getNextPageWhenSingleAnswer) match {
      case Left(value) => NotFound(s"$value")
      case Right(nextPageId) =>
        Redirect(routes.PageController.showPage(nextPageId))
    }

  }

  private def getNextPageFromDiagramNode(
      findFinalPage: Either[PageControllerError, DiagramNode]
  ): Either[PageControllerError, Result] = {
    for {
      diagramNode <- findFinalPage
      content <- diagramNode.value match {
        case Some(value) => Right(value)
        case None =>
          Left(XMLParsingError(diagramNode))
      }
    } yield Ok(views.html.result(content))
  }

  private def findIdFromPageData(
      pageId: Int
  ): Either[IdNotFoundInPageRoutingData, PageRoutingAndQuestions] = {
    PageController.pageRoutingData.find(_.pageTitle.id == pageId) match {
      case Some(value) => Right(value)
      case None        => Left(IdNotFoundInPageRoutingData(pageId))
    }
  }
}

object PageController {

  private val filePath: String = "conf/flowChartTest.xml"

  private val xml: Elem = XML.loadFile(filePath)

  val diagramNodes: Seq[DiagramNode] =
    (xml \ "diagram" \ "mxGraphModel" \ "root" \ "mxCell").map(x =>
      DiagramNode.fromXml(x)
    )

  val pageRoutingData: Seq[PageRoutingAndQuestions] = {
    DiagramNodeProcessor.getSourceNodes(diagramNodes)
  }

}

abstract class PageControllerError(message: String)
    extends RuntimeException(message)

case class MoreThanOneAnswersError(id: Int, answers: Seq[String])
    extends PageControllerError(
      s"Did not find a single answer for id: $id. Found ${answers.mkString(",")}"
    )

case class GetAnswersError(id: Int)
    extends PageControllerError(
      s"Could not find 'answers[]' in the request fo id: $id"
    )
case class AsFormUrlEncoded(id: Int)
    extends PageControllerError(
      s"Did not get anything from the asFormUrlEncoded for id: $id"
    )

case class XMLParsingError(diagramNode: DiagramNode)
    extends PageControllerError(s"Did not receive a value for $diagramNode")
case class IdNotFoundInDiagramNodes(id: Int)
    extends PageControllerError(
      s"The page with id: $id was not found in the diagramNodes"
    )
case class IdNotFoundInPageRoutingData(id: Int)
    extends PageControllerError(
      s"The page with id: $id was not found in the PageRoutingAndQuestions"
    )
