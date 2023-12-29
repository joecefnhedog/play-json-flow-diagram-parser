package controllers

import play.api.Configuration
import play.api.mvc._
import routing.PageRoutingAndQuestions
import xml.parser.DiagramNode
import xml.reader.WAATPXMLReader

import javax.inject._

@Singleton
class PageController @Inject() (
    cc: ControllerComponents,
    configuration: Configuration
) extends AbstractController(cc)
    with WAATPXMLReader {

  override def filePath: String = {
    configuration.get[String]("who.ate.all.the.pies.flowDiagram")
  }

  def showPage(id: Int): Action[AnyContent] = Action {
    findIdFromPageData(id)
      .map(pageData => Ok(views.html.showPage(pageData)))
      .orElse(
        findFinalPage(id).flatMap(getNextPageFromDiagramNode)
      ) match {
      case Left(value)  => NotFound(s"$value")
      case Right(value) => value
    }

  }

  def processAnswer(pageId: Int): Action[AnyContent] = Action { request =>
    val destination: Either[PageControllerError, Int] = for {
      bodyForm <- request.body.asFormUrlEncoded.toRight(
        AsFormUrlEncoded(pageId)
      )
      answers <- bodyForm.get("answers[]").toRight(GetAnswersError(pageId))
      pageRoutingAndQuestions <- findIdFromPageData(pageId)
      matchAnswer <- answers match {
        case singleAnswer :: Nil => Right(singleAnswer)
        case _                   => Left(MoreThanOneAnswersError(pageId, answers))
      }
      answerWithRoute <- pageRoutingAndQuestions.potentialAnswers
        .find(
          _.ans == matchAnswer
        )
        .toRight(IdNotFoundInPageRoutingData(pageId))

    } yield answerWithRoute.destination

    val getNextPageWhenSingleAnswer: Either[PageControllerError, Int] = for {
      pageRoutingAndQuestions <- findIdFromPageData(pageId)
      destination <- pageRoutingAndQuestions.matchAnswerToDestination(pageId)
    } yield destination

    destination.orElse(getNextPageWhenSingleAnswer) match {
      case Left(value) => NotFound(s"$value")
      case Right(nextPageId) =>
        Redirect(routes.PageController.showPage(nextPageId))
    }

  }

  private def getNextPageFromDiagramNode(
      diagramNode: DiagramNode
  ): Either[PageControllerError, Result] =
    for {
      content <- diagramNode.value.toRight(XMLParsingError(diagramNode))
    } yield Ok(views.html.result(content))

  private def findIdFromPageData(
      pageId: Int
  ): Either[IdNotFoundInPageRoutingData, PageRoutingAndQuestions] =
    pageRoutingData
      .find(_.pageTitle.id == pageId)
      .toRight(IdNotFoundInPageRoutingData(pageId))

  private def findFinalPage(id: Int): Either[PageControllerError, DiagramNode] =
    diagramNodes
      .find(_.id == id)
      .toRight(IdNotFoundInDiagramNodes(id))

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
