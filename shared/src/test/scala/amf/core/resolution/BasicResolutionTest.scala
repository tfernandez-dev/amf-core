package amf.core.resolution

import amf.client.convert.{BaseUnitConverter, NativeOps}
import amf.client.model.document.Document
import amf.client.resolve.AmfGraphResolver
import amf.core.io.FileAssertionTest
import amf.core.model.domain.extensions.CustomDomainProperty
import amf.core.render.ElementsFixture
import org.scalatest.{FunSuite, Matchers}

trait BasicResolutionTest extends FunSuite with NativeOps with FileAssertionTest with BaseUnitConverter with Matchers with ElementsFixture{

  test("test basic link resolution"){
    val domainProperty = CustomDomainProperty().withName("myProperty").withId("amf://id6")
    document.withEncodes(domainProperty.link("myLink"))
    document.encodes.asInstanceOf[CustomDomainProperty].linkTarget.isEmpty shouldBe false
    val unit = new AmfGraphResolver().resolve(BaseUnitMatcher.asClient(document))
    unit.asInstanceOf[Document].encodes.asInstanceOf[amf.client.model.domain.CustomDomainProperty].linkTarget.asOption.isEmpty shouldBe true
  }
}
