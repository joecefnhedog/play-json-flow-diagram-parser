package xml.reader

import routing.{DiagramNodeProcessor, PageRoutingAndQuestions}
import xml.parser.DiagramNode

import scala.xml.{Elem, XML}

trait WAATPXMLReader {
  def filePath: String
  val xml: Elem = XML.loadFile(filePath)
  val diagramNodes: Seq[DiagramNode] =
    (xml \ "diagram" \ "mxGraphModel" \ "root" \ "mxCell").map(x =>
      DiagramNode.fromXml(x)
    )
  val pageRoutingData: Seq[PageRoutingAndQuestions] = {
    DiagramNodeProcessor.getSourceNodes(diagramNodes)
  }

}
