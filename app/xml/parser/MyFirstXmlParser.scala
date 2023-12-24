package xml.parser

import scala.xml._

object MyFirstXmlParser extends App {

  val filePath = "conf/flowChartTest.xml"

  val xml = XML.loadFile(filePath)

  val diagramNodes = (xml \ "diagram" \ "mxGraphModel" \ "root" \ "mxCell").map(x=> DiagramNode.fromXml(x))

  diagramNodes.foreach(println)
}


case class DiagramNode(id: String, value: String, style: String, vertex: Boolean, geometry: Geometry)

case class Geometry(x: Option[Double], y: Option[Double], width: Option[Double], height: Option[Double])

object DiagramNode {
  def fromXml(xmlNode: Node): DiagramNode = {
    val id = (xmlNode \ "@id").text
    val value = (xmlNode \ "@value").text
    val style = (xmlNode \ "@style").text
    val vertex = (xmlNode \ "@vertex").text.toBooleanOption.getOrElse(false)

    val geometryNode = xmlNode \ "mxGeometry"
    val geometry = Geometry(
      x = (geometryNode \ "@x").headOption.map(_.text.toDouble),
      y = (geometryNode \ "@y").headOption.map(_.text.toDouble),
      width = (geometryNode \ "@width").headOption.map(_.text.toDouble),
      height = (geometryNode \ "@height").headOption.map(_.text.toDouble)
    )

    DiagramNode(id, value, style, vertex, geometry)
  }
}