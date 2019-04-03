package amf.core.resolution

import amf.Core
import amf.client.convert.{BaseUnitConverter, NativeOps}
import amf.client.model.document.Document
import amf.client.resolve.AmfGraphResolver
import amf.core.io.FileAssertionTest
import amf.core.model.domain.extensions.CustomDomainProperty
import amf.core.render.ElementsFixture
import org.scalatest.{AsyncFunSuite, FunSuite, Matchers}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

trait BasicResolutionTest extends AsyncFunSuite with NativeOps with FileAssertionTest with BaseUnitConverter with Matchers with ElementsFixture{

  override implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  
  test("test basic link resolution"){
    Core.init().asFuture.map { _ =>

      val domainProperty = CustomDomainProperty().withName("myProperty").withId("amf://id6")
      document.withEncodes(domainProperty.link("myLink"))
      document.encodes.asInstanceOf[CustomDomainProperty].linkTarget.isEmpty shouldBe false
      val unit = new AmfGraphResolver().resolve(BaseUnitMatcher.asClient(document))
      unit.asInstanceOf[Document].encodes.asInstanceOf[amf.client.model.domain.CustomDomainProperty].linkTarget.asOption.isEmpty shouldBe true
    }
  }
}
