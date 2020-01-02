package amf.core.plugin

import amf.core.metamodel.document.SourceMapModel
import amf.core.parser.Annotations
import amf.core.registries.AMFDomainRegistry.defaultIri
import org.scalatest.{FunSuite, Matchers}

class PluginContextTest extends FunSuite with Matchers {


  test("Test types without blacklist") {
    val ctx = PluginContext()

    CorePlugin.modelEntities.foreach { `type` =>
      val iri = defaultIri(`type`)
      shouldBeDefined(ctx.findType(iri))
    }

    CorePlugin.modelEntities.filterNot(_ == SourceMapModel).foreach { `type` =>
      val builder = ctx.buildType(`type`)
      val instance = builder(Annotations())
      instance.meta should be(`type`)
    }
  }

  test("Test types with blacklist") {
    val ctx = PluginContext(Seq(CorePlugin))

    CorePlugin.modelEntities.foreach { `type` =>
      val iri = defaultIri(`type`)
      shouldBeEmpty(ctx.findType(iri))
    }

    CorePlugin.modelEntities.foreach { `type` =>
      the[Exception] thrownBy {
        ctx.buildType(`type`)
      } should have message s"Cannot find builder for type ${`type`}"
    }
  }

  private def shouldBeDefined[T](opt: Option[T]): Unit = {
    //  should be ('defined) not working on JS
    opt.isDefined should be(true)
  }

  private def shouldBeEmpty[T](opt: Option[T]): Unit = {
    //  should be ('empty) not working on JS
    opt.isDefined should be(false)
  }
}
