import Magnificent2Spec._
import org.scalatest._
import shapeless._
import shapeless.ops.hlist._

import syntax.std.tuple._


object Magnificent2Spec {

  final case class Title(title: String)

  final case class JsonLink(jsonLink: String)

  final case class Content(content: String)

  final case class RenderingParameters(title: String,
                                       jsonLink: Option[String],
                                       stringContent: String)


  def buildParameters[T <: HList](t: T)(implicit titleSelector: Selector[T, Title],
                                        contentSelector: Selector[T, Content],
                                        optionalJsonLinkSelector: OptionalSelector[T, JsonLink],
  ): RenderingParameters = {
    RenderingParameters(
      title = t.select[Title].title,
      jsonLink = optionalJsonLinkSelector.apply(t).map(_.jsonLink),
      stringContent = t.select[Content].content,
    )
  }
}

final class Magnificent2Spec extends FreeSpec {
  "rendered" - {
    "Renders with JSON link" in {
      val input = Title("Hello") :: JsonLink("/some.json") :: Content("This is our result") :: HNil
      val expectedParameters = RenderingParameters("Hello", Some("/some.json"), "This is our result")
      assert(buildParameters(input) == expectedParameters)
    }
    "Renders without JSON link" in {
      val input = Title("Hello") :: Content("This is our result") :: HNil
      val expectedParameters = RenderingParameters("Hello", None, "This is our result")
      assert(buildParameters(input) == expectedParameters)
    }
  }
}

