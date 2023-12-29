package routing

import controllers.{IdNotFoundInPageRoutingData, MoreThanOneAnswersError}
import xml.parser.DiagramNode

object DiagramNodeProcessor extends App {

  def getSourceNodes(nodes: Seq[DiagramNode]): Seq[PageRoutingAndQuestions] = {

    val nodesByParentFiltered: Map[Int, Seq[DiagramNode]] =
      nodes.groupBy(_.source).collect { case (Some(parent), nodes) =>
        (parent, nodes)
      }

    nodesByParentFiltered.map { case (source, nodesPointingToSource) =>
      PageRoutingAndQuestions(
        pageTitle = PageInfo(
          title = pageTitle(nodes, source),
          id = source
        ),
        potentialAnswers = getAnswers(nodesPointingToSource)
      )

    }.toSeq

  }

  private def pageTitle(nodes: Seq[DiagramNode], source: Int): String = {
    nodes
      .find(_.id == source)
      .getOrElse(throw new RuntimeException("lost the id"))
      .value
      .getOrElse(throw new RuntimeException("got no value"))
  }

  private def getAnswers(
      nodesPointingToSource: Seq[DiagramNode]
  ): Seq[AnswerWithRoute] = {
    nodesPointingToSource.map(diagramNode =>
      AnswerWithRoute(
        ans = diagramNode.value.getOrElse(
          throw new RuntimeException("lost the value")
        ),
        destination = diagramNode.target.getOrElse(
          throw new RuntimeException("got no target")
        )
      )
    )
  }
}

case class AnswerWithRoute(ans: String, destination: Int)

case class PageInfo(title: String, id: Int)

case class PageRoutingAndQuestions(
    pageTitle: PageInfo,
    potentialAnswers: Seq[AnswerWithRoute]
) {

  def answerWithRoute(
      matchAnswer: String,
      pageId: Int
  ): Either[IdNotFoundInPageRoutingData, AnswerWithRoute] = potentialAnswers
    .find(
      _.ans == matchAnswer
    )
    .toRight(IdNotFoundInPageRoutingData(pageId))

  def matchAnswerToDestination(
      pageId: Int
  ): Either[MoreThanOneAnswersError, Int] =
    potentialAnswers match {
      case singleAnswer :: Nil => Right(singleAnswer.destination)
      case _ =>
        Left(
          controllers.MoreThanOneAnswersError(
            pageId,
            potentialAnswers.map(_.ans)
          )
        )
    }

}
