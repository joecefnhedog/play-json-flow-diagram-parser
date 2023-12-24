package xml.parser

import scala.xml._

object MyFirstXmlParser extends App {

  val filePath = "conf/flowChartTest.xml"

  val xml = XML.loadFile(filePath)

  val diagramNodes =
    (xml \ "diagram" \ "mxGraphModel" \ "root" \ "mxCell").map(x =>
      DiagramNode.fromXml(x)
    )

  diagramNodes.foreach(println)
}

case class DiagramNode(
    id: String,
    value: Option[String],
    style: String,
    vertex: Boolean,
    geometry: Geometry,
    edge: Option[String],
    source: Option[String],
    target: Option[String],
    parent: String
){
  override def toString: String = {
    s"DiagramNode(id=$id, value=$value, vertex=$vertex, geometry=<excluded>, " +
      s"edge=$edge, source=$source, target=$target, parent=$parent)"
  }
}

case class Geometry(
    x: Option[Double],
    y: Option[Double],
    width: Option[Double],
    height: Option[Double]
)

object DiagramNode {
  def fromXml(xmlNode: Node): DiagramNode = {
    val id = (xmlNode \ "@id").text
    val value = (xmlNode \ "@value").headOption.map(_.text)
    val style = (xmlNode \ "@style").text
    val vertex = (xmlNode \ "@vertex").text.toBooleanOption.getOrElse(false)
    val edge = (xmlNode \ "@edge").headOption.map(_.text)
    val source = (xmlNode \ "@source").headOption.map(_.text)
    val target = (xmlNode \ "@target").headOption.map(_.text)
    val parent = (xmlNode \ "@parent").text

    val geometryNode = xmlNode \ "mxGeometry"
    val geometry = Geometry(
      x = (geometryNode \ "@x").headOption.map(_.text.toDouble),
      y = (geometryNode \ "@y").headOption.map(_.text.toDouble),
      width = (geometryNode \ "@width").headOption.map(_.text.toDouble),
      height = (geometryNode \ "@height").headOption.map(_.text.toDouble)
    )

    DiagramNode(id, value, style, vertex, geometry,edge, source, target, parent)
  }
}
