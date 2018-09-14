import shapeless._
import scala.language.higherKinds

trait GenericSelector[M[_], L <: HList, V] extends DepFn1[L] with Serializable {
  type Out = M[V]
}

object GenericSelector {
  def apply[M[_], L <: HList, U](l: L)(
      implicit genericSelector: GenericSelector[M, L, U])
    : GenericSelector[M, L, U] = genericSelector

  implicit def select[M[_], H, T <: HList]: GenericSelector[M, M[H] :: T, H] =
    new GenericSelector[M, M[H] :: T, H] {
      def apply(l: M[H] :: T): M[H] = l.head
    }

  implicit def recurse[M[_], HH, H, T <: HList, U](
      implicit extractNext: GenericSelector[M, H :: T, U])
    : GenericSelector[M, HH :: H :: T, U] =
    new GenericSelector[M, HH :: H :: T, U] {
      override def apply(t: HH :: H :: T): M[U] =
        extractNext.apply(t.tail)
    }
}
