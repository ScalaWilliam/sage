import Magnificent3Spec._
import org.scalatest._
import shapeless._
import shapeless.syntax.std.tuple._

import scala.language.higherKinds

object Magnificent3Spec {

  final case class Wrapper[T](value: T)

}

final class Magnificent3Spec extends FreeSpec {

  "GenericSelector" - {
    val wrapperSelector = GenericSelector[Wrapper]

    "Extracts head" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult = wrapperSelector(wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    "Extracts it as a second item" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult = wrapperSelector(1 :: wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    // this does not compile :-(
    "Extracts it as a third item" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult =
        wrapperSelector(1 :: "Test" :: wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

  }
}
