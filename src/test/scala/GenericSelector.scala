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

    implicit def recurse[H, T <: HList, U](
        implicit extractNext: Aux[M[U] :: T, U]): Aux[H :: M[U] :: T, U] =
      new Aux[H :: M[U] :: T, U] {
        override def apply(t: H :: M[U] :: T): M[U] =
          extractNext.apply(t.tail)
      }

  }
}
