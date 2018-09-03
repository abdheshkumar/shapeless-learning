package level2

import shapeless.Generic

case class A(a: Int, b: Double, c: String, d: Boolean)

object Example2 extends App {
  // (Int, Double, String, Boolean) <=> Int :: Double :: String :: Boolean :: HNil
  val gen = Generic[(Int, Double, String, Boolean)]

  val l = gen.to((1, 3.14, "foo", true)) // 1 :: 3.14 :: "foo" :: true :: HNil
  val t = gen.from(l) // (1, 3.14, "foo", true)
  val a = Generic[A]
  println(a.from(l))
}
