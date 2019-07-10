package amf.core.iterator
import amf.core.metamodel.document.FragmentModel
import amf.core.metamodel.domain.DataNodeModel
import amf.core.model.domain.{AmfObject, DomainElement}
import amf.core.vocabulary.Namespace
import com.mule.data.DataNodes
import org.scalatest.{FunSuite, Matchers}

trait IteratorTest extends FunSuite with Matchers {

  test("Complete iterator (simple document)") {

    val it = CompleteStrategy.iterator(DataNodes.document)
    it.size should be(14)

  }

  test("Complete iterator (recursive fragment)") {

    val it = CompleteStrategy.iterator(DataNodes.fragment)
    it.size should be(9)

  }

  test("Domain element iterator (recursive fragment) collect") {
    val ids =
      DataNodes.fragment.collect() { case e: DomainElement => e.id }.toStream

    ids should contain inOrderOnly ("amf://recursive", "amf://name", "amf://age")
  }

  test("Domain element iterator (recursive fragment) collect first") {
    val location =
      DataNodes.fragment.collectFirst(CompleteStrategy) {
        case obj: AmfObject if obj.fields.?(FragmentModel.Location).isDefined =>
          obj.fields.get(FragmentModel.Location).toString
      }

    location shouldBe Some("http://fragment")
  }
}
