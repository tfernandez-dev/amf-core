package amf.core.model
import amf.core.annotations.{LexicalInformation, SourceVendor}
import amf.core.model.document.Document
import amf.core.model.domain.{ArrayNode, ObjectNode, ScalarNode}
import amf.core.remote.Raml10
import amf.core.render.ElementsFixture
import org.scalatest.{FunSuite, Matchers}

class ModelCloneTest extends FunSuite with ElementsFixture with Matchers{

  test("Test clone encoded at Document"){
    val cloned :Document = document.cloneUnit().asInstanceOf[Document]
    cloned.encodes.asInstanceOf[ScalarNode].withValue("ClonedValue")

    document.encodes.asInstanceOf[ScalarNode].value.value() should be("myValue")
    cloned.encodes.asInstanceOf[ScalarNode].value.value() should be("ClonedValue")
  }

  test("Test clone object node"){
    val cloned: ObjectNode = objectNode.cloneElement(Map.empty).asInstanceOf[ObjectNode]

    cloned.addProperty("newProp",ScalarNode("newValue",Some(DataType.String)))

    objectNode.allProperties().size should be(1)
    cloned.allProperties().size should be(2)

    cloned.allPropertiesWithName()("myProp1").asInstanceOf[ArrayNode].addMember(ScalarNode("new2", Some(DataType.String)))

    arrayNode.members.length should be(1)
    objectNode.allPropertiesWithName()("myProp1").asInstanceOf[ArrayNode].members.length should be(1)
    cloned.allPropertiesWithName()("myProp1").asInstanceOf[ArrayNode].members.length should be(2)
  }


  test("Test clone recursive object"){
    val cloned = recursiveObj.cloneElement(Map.empty).asInstanceOf[ObjectNode]

    cloned.allProperties().head.asInstanceOf[ArrayNode].members.head.asInstanceOf[ObjectNode].allProperties().head should be(cloned)
    cloned.allProperties().head.asInstanceOf[ArrayNode].members.head.asInstanceOf[ObjectNode].id should be(recursiveObj.allProperties().head.asInstanceOf[ArrayNode].members.head.asInstanceOf[ObjectNode].id)
    succeed
  }

  test("Test annotations at object"){
    objectNode.annotations += SourceVendor(Raml10)
    val cloned = objectNode.cloneElement(Map.empty).asInstanceOf[ObjectNode]

    cloned.annotations.contains(classOf[SourceVendor]) should be(true)
  }

}
