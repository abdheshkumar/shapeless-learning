import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

sealed trait Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {

  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc


  // "Constructor" method
  def instance[A](f: A => List[String]) = new CsvEncoder[A] {
    override def encode(value: A): List[String] = f(value)
  }
}

implicit val stringEncoder: CsvEncoder[String] =
  CsvEncoder.instance(str => List(str))
implicit val intEncoder: CsvEncoder[Int] =
  CsvEncoder.instance(num => List(num.toString))
implicit val booleanEncoder: CsvEncoder[Boolean] =
  CsvEncoder.instance(bool => List(if (bool) "yes" else "no"))
implicit val doubleEncoder: CsvEncoder[Double] =
  CsvEncoder.instance(d => List(d.toString))
implicit val hnilEncoder: CsvEncoder[HNil] =
  CsvEncoder.instance(hnil => Nil)
implicit val cnilEncoder: CsvEncoder[CNil] =
  CsvEncoder.instance(cnil => throw new Exception("Inconceivable!"))
implicit def hlistEncoder[H, T <: HList](
                                          implicit
                                          hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
                                          tEncoder: CsvEncoder[T]
                                        ): CsvEncoder[H :: T] = CsvEncoder.instance {
  case h :: t =>
    hEncoder.value.encode(h) ++ tEncoder.encode(t)
}
implicit def coproductEncoder[H, T <: Coproduct](
                                                  implicit
                                                  hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
                                                  tEncoder: CsvEncoder[T]
                                                ): CsvEncoder[H :+: T] = CsvEncoder.instance {
  case Inl(h) => hEncoder.value.encode(h)
  case Inr(t) => tEncoder.encode(t)
}
implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic.Aux[A, R],
                                   rEncoder: Lazy[CsvEncoder[R]] // wrap in Lazy
                                 ): CsvEncoder[A] = CsvEncoder.instance { value =>
  rEncoder.value.encode(gen.to(value))
}

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")

CsvEncoder[Tree[Int]]
