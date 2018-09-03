import shapeless.{::, Generic, HList, HNil}

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

/*
Given a type A and an HList type R , an implicit Generic to map A
to R , and a CsvEncoder for R , create a CsvEncoder for A .
 */
implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic[A] {type Repr = R}, //Search for implicit which will convert `A` type to HList
                                   enc: CsvEncoder[R] //Search for implicit which will convert HList to CsvEncoder
                                 ): CsvEncoder[A] =
  createEncoder(a => enc.encode(gen.to(a)))

//Aux pattern
/*implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic.Aux[A, R], //Search for implicit which will convert `A` type to HList
                                   enc: CsvEncoder[R] //Search for implicit which will convert HList to CsvEncoder
                                 ): CsvEncoder[A] =
  createEncoder(a => enc.encode(gen.to(a)))*/

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)

//writeCsv(iceCreams) // This is same as below

writeCsv(iceCreams)(
  genericEncoder(
    Generic[IceCream],
    hlistEncoder(stringEncoder,
      hlistEncoder(intEncoder,
        hlistEncoder(booleanEncoder, hnilEncoder)))))