import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.Last

def getRepr[A](value: A)(implicit gen: Generic[A]): gen.Repr =
  gen.to(value)

val last1 = Last[String :: Int :: HNil]
val last2 = Last[Int :: String :: HNil]

last1("foo" :: 123 :: HNil)
last2(321 :: "bar" :: HNil)

trait Second[L <: HList] {
  type Out

  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] {type Out = O}

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

  implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B

      def apply(value: A :: B :: Rest): B =
        value.tail.head
    }
}


val second1 = Second[String :: Boolean :: Int :: HNil]
val second2 = Second[String :: Int :: Boolean :: HNil]

second1("foo" :: true :: 123 :: HNil)
second2("bar" :: 321 :: false :: HNil)

//second1("baz" :: HNil)
case class Vec(x: Int, y: Int)

case class Rect(origin: Vec, size: Vec)

def lastField[A, Repr <: HList](input: A)(
  implicit
  gen: Generic.Aux[A, Repr],
  last: Last[Repr]
): last.Out = last.apply(gen.to(input))
lastField(Rect(Vec(1, 2), Vec(3, 4)))

val theAnswer: 42 = 42
