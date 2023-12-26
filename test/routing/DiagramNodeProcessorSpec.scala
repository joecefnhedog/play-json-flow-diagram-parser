package routing

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import xml.parser.DiagramNode

import scala.xml.XML

class DiagramNodeProcessorSpec extends AnyFlatSpec with Matchers {

  private val filePath = "conf/flowChartTest.xml"

  private val xml = XML.loadFile(filePath)

  private val diagramNodes =
    (xml \ "diagram" \ "mxGraphModel" \ "root" \ "mxCell").map(x =>
      DiagramNode.fromXml(x)
    )

  "DiagramNodeProcessor" should "correctly process nodes" in {

    val result: Seq[PageRoutingAndQuestions] =
      DiagramNodeProcessor.getSourceNodes(diagramNodes)

    val expected: Seq[PageRoutingAndQuestions] = Seq(
      PageRoutingAndQuestions(
        PageInfo("Lamp doesn't work", 3),
        Seq(AnswerWithRoute("", 6))
      ),
      PageRoutingAndQuestions(
        PageInfo("Lamp<br>plugged in?", 6),
        Seq(AnswerWithRoute("Yes", 10), AnswerWithRoute("No", 7))
      ),
      PageRoutingAndQuestions(
        PageInfo("Bulb<br>burned out?", 10),
        Seq(AnswerWithRoute("No", 11), AnswerWithRoute("Yes", 12))
      )
    )

    result should contain theSameElementsAs expected

  }

}
