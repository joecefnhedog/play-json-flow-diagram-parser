package xml.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.xml.XML

class DiagramNodeSpec extends AnyFlatSpec with Matchers {

  "DiagramNode.fromXml" should "correctly parse XML with valid boolean attribute" in {
    val xmlString =
      """
        |<mxCell id="1" value="Node" style="rounded=0;" vertex="true" edge="1" source="3" target="6" parent="1">
        |  <mxGeometry x="10" y="20" width="30" height="40"/>
        |</mxCell>
      """.stripMargin

    val xmlNode = XML.loadString(xmlString)
    val result = DiagramNode.fromXml(xmlNode)

    result shouldEqual DiagramNode(
      "1",
      Some("Node"),
      "rounded=0;",
      true,
      Geometry(Some(10.0), Some(20.0), Some(30.0), Some(40.0)),
      edge = Some("1"),
      source = Some("3"),
      target = Some("6"),
      parent = "1"
    )
  }

  it should "handle default value for boolean attribute when not present" in {
    val xmlString =
      """
        |<mxCell id="2" value="Another Node" style="rounded=1;" edge="1" source="3" target="6" parent="1">
        |  <mxGeometry x="50" y="60" width="70" height="80"/>
        |</mxCell>
      """.stripMargin

    val xmlNode = XML.loadString(xmlString)
    val result = DiagramNode.fromXml(xmlNode)

    result shouldEqual DiagramNode(
      "2",
      Some("Another Node"),
      "rounded=1;",
      false,
      Geometry(Some(50.0), Some(60.0), Some(70.0), Some(80.0)),
      edge = Some("1"),
      source = Some("3"),
      target = Some("6"),
      parent = "1"
    )
  }

  it should "handle empty string for boolean attribute" in {
    val xmlString =
      """
        |<mxCell id="3" value="Yet Another Node" style="rounded=1;" vertex="" edge="1" source="3" target="6" parent="1">
        |  <mxGeometry x="90" y="100" width="110" height="120"/>
        |</mxCell>
      """.stripMargin

    val xmlNode = XML.loadString(xmlString)
    val result = DiagramNode.fromXml(xmlNode)

    result shouldEqual DiagramNode(
      "3",
      Some("Yet Another Node"),
      "rounded=1;",
      false,
      Geometry(Some(90.0), Some(100.0), Some(110.0), Some(120.0)),
      edge = Some("1"),
      source = Some("3"),
      target = Some("6"),
      parent = "1"
    )
  }

  it should "handle missing geometry attributes" in {
    val xmlString =
      """
        |<mxCell id="4" value="Node Without Geometry" style="rounded=1;" vertex="true" edge="1" source="3" target="6" parent="1">
        |</mxCell>
      """.stripMargin

    val xmlNode = XML.loadString(xmlString)
    val result = DiagramNode.fromXml(xmlNode)

    result shouldEqual DiagramNode(
      "4",
      Some("Node Without Geometry"),
      "rounded=1;",
      true,
      Geometry(None, None, None, None),
      edge = Some("1"),
      source = Some("3"),
      target = Some("6"),
      parent = "1"
    )
  }
}
