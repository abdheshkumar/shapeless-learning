import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
  new CsvEncoder[A] {
    def encode(value: A): List[String] = func(value)
  }

implicit val stringEncoder: CsvEncoder[String] =
  createEncoder(str => List(str))
implicit val intEncoder: CsvEncoder[Int] =
  createEncoder(num => List(num.toString))
implicit val booleanEncoder: CsvEncoder[Boolean] =
  createEncoder(bool => List(if (bool) "yes" else "no"))
implicit val doubleEncoder: CsvEncoder[Double] =
  createEncoder(d => List(d.toString))
implicit val hnilEncoder: CsvEncoder[HNil] =
  createEncoder(hnil => Nil)

implicit def hlistEncoder[H, T <: HList](
                                          implicit
                                          hEncoder: CsvEncoder[H],
                                          tEncoder: CsvEncoder[T]
                                        ): CsvEncoder[H :: T] = createEncoder {
  case h :: t =>
    hEncoder.encode(h) ++ tEncoder.encode(t)
}

implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic.Aux[A, R], //Search for implicit which will convert `A` type to HList
                                   enc: CsvEncoder[R] //Search for implicit which will convert HList to CsvEncoder
                                 ): CsvEncoder[A] =
  createEncoder(a => enc.encode(gen.to(a)))

implicit val cnilEncoder: CsvEncoder[CNil] =
  createEncoder(cnil => throw new Exception("Inconceivable!"))

implicit def coproductEncoder[H, T <: Coproduct](
                                                  implicit
                                                  hEncoder: CsvEncoder[H],
                                                  tEncoder: CsvEncoder[T]
                                                ): CsvEncoder[H :+: T] = createEncoder {
  case Inl(h) => hEncoder.encode(h)
  case Inr(t) => tEncoder.encode(t)
}

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")

val shapes: List[Shape] = List(
  Rectangle(3.0, 4.0),
  Circle(1.0)
)
writeCsv(shapes)