package level2

import shapeless.Generic.Aux
import shapeless.{Generic, HList, HNil}

object Example3 extends App {
  def flip[T, L <: HList](x: T)
                         (implicit gen: Generic.Aux[T, L], f: Flip[L]): T =
    gen.from(f(gen.to(x)))

  // T = (Int, Double, String, Boolean)
  // L = Int :: Double :: String :: Boolean :: HNil
  // gen = Generic[(Int, Double, String, Boolean)]
  // f = implicitly[Flip[Int :: Double :: String :: Boolean :: HNil]]

  flip((1, 3.14, "foo", true))



  // Record <=> Int :: Double :: String :: Boolean :: HNil
  val gen = Generic.apply[Record]
  val l = gen.to(Record(1, 3.14, "foo", true)) // 1 :: 3.14 :: "foo" :: true :: HNil
  val g = gen.from(l) // Record(1, 3.14, "foo", true)

  flip(Record(1, 3.14, "foo", true)) // Record(-1, -3.14, "oof", false)
}
case class Record(i: Int, d: Double, s: String, b: Boolean)