package routing

import controllers.{IdNotFoundInPageRoutingData, MoreThanOneAnswersError}
import play.api.mvc.{PathBindable, QueryStringBindable}
import xml.parser.DiagramNode

import scala.collection.immutable

object DiagramNodeProcessor {

  def getSourceNodes(nodes: Seq[DiagramNode]): Seq[PageRoutingAndQuestions] = {

    val nodesByParentFiltered =
      nodes.groupBy(_.source)

    val collectSome = nodesByParentFiltered.collect {
      case (Some(parent), nodes) =>
        (parent, nodes)
    }

    val collectNone: immutable.Iterable[DiagramNode] = nodesByParentFiltered
      .collect {
        case x if x._1.isEmpty =>
          (x._2)
      }
      .flatten
      .filter(_.value.isDefined)

    collectSome.map { case (source, nodesPointingToSource) =>
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

object AnswerWithRoute {

  implicit def queryStringBindable: QueryStringBindable[AnswerWithRoute] =
    new QueryStringBindable[AnswerWithRoute] {
      override def bind(
          key: String,
          params: Map[String, Seq[String]]
      ): Option[Either[String, AnswerWithRoute]] = {
        for {
          ans <- params.get(s"$key[ans]").flatMap(_.headOption)
          dest <- params
            .get(s"$key[destination]")
            .flatMap(_.headOption)
            .map(_.toInt)
        } yield Right(AnswerWithRoute(ans, dest))
      }

      override def unbind(key: String, value: AnswerWithRoute): String =
        s"$key[ans]=${value.ans}&$key[destination]=${value.destination}"
    }

  implicit def seqQueryStringBindable
      : QueryStringBindable[Seq[AnswerWithRoute]] =
    new QueryStringBindable[Seq[AnswerWithRoute]] {
      override def bind(
          key: String,
          params: Map[String, Seq[String]]
      ): Option[Either[String, Seq[AnswerWithRoute]]] = {
        val ansValues = params.getOrElse(s"$key[ans]", Seq.empty)
        val destValues = params.getOrElse(s"$key[destination]", Seq.empty)

        val result = ansValues
          .zip(destValues)
          .foldLeft[Option[Seq[AnswerWithRoute]]](Some(Seq.empty)) {
            case (acc, (ans, dest)) =>
              for {
                answers <- acc
                route <- AnswerWithRoute.queryStringBindable
                  .bind("", Map("ans" -> Seq(ans), "destination" -> Seq(dest)))
                  .flatMap(_.toOption)
              } yield answers :+ route
          }

        result.map(Right.apply)
      }

      override def unbind(key: String, value: Seq[AnswerWithRoute]): String =
        value.zipWithIndex
          .map { case (route, index) =>
            AnswerWithRoute.queryStringBindable.unbind(s"$key[$index]", route)
          }
          .mkString("&")
    }

  implicit def pathBindable: PathBindable[AnswerWithRoute] =
    new PathBindable[AnswerWithRoute] {
      override def bind(
          key: String,
          value: String
      ): Either[String, AnswerWithRoute] = {
        val parts = value.split(",")
        if (parts.length == 2) {
          try {
            val ans = parts(0)
            val dest = parts(1).toInt
            Right(AnswerWithRoute(ans, dest))
          } catch {
            case _: NumberFormatException =>
              Left(s"Cannot parse $value as an integer for $key")
          }
        } else {
          Left(s"Invalid format for $key: $value")
        }
      }

      override def unbind(key: String, value: AnswerWithRoute): String =
        s"${value.ans},${value.destination}"
    }

  implicit def pathBindableSeq: PathBindable[Seq[AnswerWithRoute]] = new PathBindable[Seq[AnswerWithRoute]] {
    override def bind(key: String, value: String): Either[String, Seq[AnswerWithRoute]] = {
      val answerStrings = value.split(";")
      val answerSeq = answerStrings.flatMap { answerString =>
        AnswerWithRoute.pathBindable.bind("", answerString) match {
          case Right(answer) => Some(answer)
          case Left(error) => None
        }
      }
      Right(answerSeq)
    }

    override def unbind(key: String, value: Seq[AnswerWithRoute]): String =
      value.map(AnswerWithRoute.pathBindable.unbind("", _)).mkString(";")
  }
}

case class PageInfo(title: String, id: Int)

case class PageRoutingAndQuestions(
    pageTitle: PageInfo,
    potentialAnswers: Seq[AnswerWithRoute]
) {

}
