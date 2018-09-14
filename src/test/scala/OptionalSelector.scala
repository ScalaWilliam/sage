import shapeless._

trait OptionalSelector[L <: HList, U] extends DepFn1[L] with Serializable {
  type Out = Option[U]
}

object OptionalSelector {

  def apply[L <: HList, U](implicit optionalSelector: OptionalSelector[L, U]): OptionalSelector[L, U] = optionalSelector

  implicit def hnilSelectOptional[L <: HList, T]: OptionalSelector[L, T] =
    new OptionalSelector[L, T] {
      def apply(l: L): Out = None
    }

  implicit def select[H, T <: HList]: OptionalSelector[H :: T, H] =
    new OptionalSelector[H :: T, H] {
      def apply(l: H :: T) = Some(l.head)
    }

  implicit def recurse[H, T <: HList, U]
  (implicit st: OptionalSelector[T, U]): OptionalSelector[H :: T, U] =
    new OptionalSelector[H :: T, U] {
      def apply(l: H :: T) = st(l.tail)
    }
}