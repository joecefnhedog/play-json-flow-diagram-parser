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

    val findFinalPage: Option[DiagramNode] = PageController.diagramNodes.find(_.id.toInt == id)

    findIdFromPageData(id)
      .map(pageData => Ok(views.html.showPage(pageData)))
      .orElse(
        getFinalPage(findFinalPage)
      )
      .getOrElse(NotFound(s"Page with id $id not found."))

  }

  def processAnswer(pageId: Int): Action[AnyContent] = Action { request =>
    val destination: Option[Int] = for {
      a <- request.body.asFormUrlEncoded
      b <- a.get("answers[]")
      c <- findIdFromPageData(pageId)
      d <- b match {
        case singleAnswer :: Nil => Some(singleAnswer)
        case _                   => None
      }
      e <- c.potentialAnswers.find(_.ans == d)

    } yield e.destination

    val getNextPageWhenSingleAnswer: Option[Int] = for {
      a <- findIdFromPageData(pageId)
      b <-  a.potentialAnswers match {
        case singleAnswer :: Nil => Some (singleAnswer.destination)
        case _ => None
      }
    } yield b

    destination.orElse(getNextPageWhenSingleAnswer) match {
      case Some(value) => Redirect(routes.PageController.showPage(value))
      case None        => NotFound(s"No valid destination found from $pageId")
    }

  }

  private def getFinalPage(findFinalPage: Option[DiagramNode]): Option[Result] = {
    for {
      a <- findFinalPage
      b <- a.value
    } yield Ok(views.html.result(b))
  }

  private def findIdFromPageData(pageId: Int): Option[PageRoutingAndQuestions] = {
    PageController.pageRoutingData.find(_.pageTitle.id == pageId)
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
