import shapeless._
import scala.language.higherKinds

object GenericSelector {
  def apply[M[_]]: GenericSelector[M] = new GenericSelector[M]
}
final class GenericSelector[M[_]] {
  trait Aux[L <: HList, V] extends DepFn1[L] with Serializable {
    type Out = M[V]
  }

  def apply[L <: HList, U](l: L)(implicit genericSelector: Aux[L, U]): M[U] = {
    genericSelector.apply(l)
  }

  object Aux {
    implicit def select[H, T <: HList]: Aux[M[H] :: T, H] =
      new Aux[M[H] :: T, H] {
        def apply(l: M[H] :: T): M[H] = l.head

      }

    implicit def recurse[HH, H, T <: HList, U](
        implicit extractNext: Aux[H :: T, U]): Aux[HH :: H :: T, U] =
      new Aux[HH :: H :: T, U] {
        override def apply(t: HH :: H :: T): M[U] =
          extractNext.apply(t.tail)
      }
  }
}
