import GenericSelectorSpec._
import org.scalatest._
import shapeless._
import shapeless.syntax.std.tuple._

import scala.language.higherKinds

object GenericSelectorSpec {

  final case class Wrapper[T](value: T)

  def extractGeneric[T <: HList, V](t: T)(
      implicit select: GenericSelector[Wrapper, T, V]): Wrapper[V] = {
    select.apply(t)
  }

}

final class GenericSelectorSpec extends FreeSpec {

  "GenericSelector" - {

    "Extracts head" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult = extractGeneric(wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    "Extracts it as a second item" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult = extractGeneric(1 :: wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    "Extracts it as a third item" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult =
        extractGeneric(1 :: "Test" :: wrapper :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    "Extracts it as a fourth item" in {
      val wrapper = Wrapper("Hello")
      val directlyInferredResult =
        extractGeneric(
          1 :: "Test" :: Option(2) :: wrapper :: "something" :: HNil)
      val ensureTypePreserved: Wrapper[String] = directlyInferredResult
      assert(ensureTypePreserved == wrapper)
    }

    "Doesn't extract" in {
//       this does not compile
//      val r = extractGeneric(1 :: HNil)
    }

  }
}