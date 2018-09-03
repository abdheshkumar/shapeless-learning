package level2

import shapeless._

object Example1 extends App {

  import Flip._

  def flip[T](x: T)(implicit f: Flip[T]) = f(x)

  flip(1 :: 3.14 :: "foo" :: true :: HNil)
//same as below
  val f = hconsFlip(intFlip,
    hconsFlip(doubleFlip,
      hconsFlip(stringFlip,
        hconsFlip(booleanFlip, hnilFlip))))
  f.apply(1 :: 3.14 :: "foo" :: true :: HNil)

  val gen = Generic[(Int, Double, String, Boolean)]

  val l = gen.to((1, 3.14, "foo", true)) // 1 :: 3.14 :: "foo" :: true :: HNil
  val t = gen.from(l)
}

trait Flip[T] {
  def apply(x: T): T
}

object Flip {
  def apply[T](f: T => T): Flip[T] = new Flip[T] {
    override def apply(x: T): T = f(x)
  }

  implicit val intFlip: Flip[Int] = Flip[Int](-_)
  implicit val doubleFlip: Flip[Double] = Flip[Double](-_)
  implicit val booleanFlip: Flip[Boolean] = Flip[Boolean](!_)
  implicit val stringFlip: Flip[String] = Flip[String](_.reverse)


  implicit val hnilFlip: Flip[HNil] = Flip[HNil](_ => HNil)

  implicit def hconsFlip[H, T <: HList]
  (implicit hf: Flip[H], tf: Flip[T]): Flip[H :: T] // summon implicit instances, tf is computed recursively
  = new Flip[H :: T] {
    override def apply(x: H :: T): H :: T = hf(x.head) :: tf(x.tail)
  }
}