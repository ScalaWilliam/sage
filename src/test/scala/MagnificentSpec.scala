import org.scalatest._
import MagnificentSpec._
import shapeless._

final class MagnificentSpec extends FreeSpec {
  "rendered" - {
    info(
      """
    
    I want to see how it's possible to build applications out of
    strongly typed inputs and only later assemble them into a concrete value.

    eg. why should I specify `jsonLink = None` when I could just
    not include the type `JsonLinked()` in the value/result type instead?
    
    This should allow me to write more concise applications.
    
    """)

    "Renders with JSON link" in {
      val input = Titled("Hello", JsonLinked("/some.json", "This is our result"))
      val expectedParameters = RenderingParameters("Hello", Some("/some.json"), "This is our result")
      assert(buildParameters(input) == expectedParameters)
    }
    "Renders without JSON link" in {
      val input = Titled("Hello", "This is our result")
      val expectedParameters = RenderingParameters("Hello", None, "This is our result")
      assert(buildParameters(input) == expectedParameters)
    }
  }
}

object MagnificentSpec {

  final case class Titled[T](title: String, content: T)

  final case class JsonLinked[T](jsonLink: String, content: T)

  final case class RenderingParameters(title: String,
                                       jsonLink: Option[String],
                                       stringContent: String)

  // ==>

  import ops.tuple.FlatMapper
  import syntax.std.tuple._
  import test._

  def buildParameters[T](t: T): RenderingParameters = {
    t match {
      case Titled(title, content: String) =>
        RenderingParameters(title = title, jsonLink = None, stringContent = content)
      case Titled(title, JsonLinked(jsonLink, content: String)) =>
        RenderingParameters(title = title, jsonLink = Some(jsonLink), stringContent = content)
    }
  }

  val sample: Titled[JsonLinked[String]] = Titled("Hello", JsonLinked("/some.json", "This is our result"))

  val sampleTransformed: Titled[JsonLinked[String]] :: JsonLinked[String] :: String :: HNil =
    sample :: sample.content :: sample.content.content :: HNil

  val renderedParameters = RenderingParameters(
    title = sampleTransformed.head.title,
    jsonLink = Some(sampleTransformed.tail.head.jsonLink),
    stringContent = sampleTransformed.tail.tail.head
  )
}