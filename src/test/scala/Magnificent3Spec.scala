import Magnificent3Spec._
import org.scalatest._
import shapeless._
import shapeless.ops.hlist.{FlatMapper, Selector}
import shapeless.syntax.std.tuple._

object Magnificent3Spec {


  final case class Wrapper[T](value: T)

  def selectGeneric[L <: HList, Inner, Outer[_]](list: L)(implicit select: Selector[L, Outer[Inner]]): Outer[Inner] = select(list)


}

final class Magnificent3Spec extends FreeSpec {
  "Map generic type" in {
    val input = Wrapper("Hello") :: HNil
    val r = selectGeneric[input.type, Wrapper[String]]
    info(s"${r}")
//    info(s"${flatten(input)}")
//    implicit val mapper: String => String = identity[String]
//    assert(renderWrapping(input) == "Hello")
  }
}

