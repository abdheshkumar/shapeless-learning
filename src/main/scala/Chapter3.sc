import shapeless.{::, Generic, HList, HNil, the}

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

case class Employee(name: String, number: Int, manager: Boolean)

object Employee {
  implicit val employee: CsvEncoder[Employee] = CsvEncoder.instance[Employee] {
    value =>
      List(
        value.name,
        value.number.toString,
        if (value.manager) "yes" else "no"
      )
  }
}

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")

val employees: List[Employee] = List(
  Employee("Bill", 1, true),
  Employee("Peter", 2, false),
  Employee("Milton", 3, false)
)

writeCsv(employees)

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

implicit val iceCreamEncoder: CsvEncoder[IceCream] =
  CsvEncoder.instance { i =>
    List(
      i.name,
      i.numCherries.toString,
      if (i.inCone) "yes" else "no"
    )
  }

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)

writeCsv(iceCreams)

implicit def pairEncoder[A, B](
                                implicit
                                aEncoder: CsvEncoder[A],
                                bEncoder: CsvEncoder[B]
                              ): CsvEncoder[(A, B)] =
  new CsvEncoder[(A, B)] {
    def encode(pair: (A, B)): List[String] = {
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }
  }

writeCsv(employees zip iceCreams)

the[CsvEncoder[IceCream]] //similar to implicitly method

def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
  new CsvEncoder[A] {
    def encode(value: A): List[String] = func(value)
  }

//val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
/*reprEncoder.encode("abc" :: 123 :: true :: HNil)*/


/*implicit val iceCreamEncoder1: CsvEncoder[IceCream] = {
  val gen = Generic[IceCream]
  val enc = CsvEncoder[gen.Repr]
  createEncoder(iceCream => enc.encode(gen.to(iceCream)))
}*/

//Single implicit resolution for all case class
/*
Given a type A and an HList type R , an implicit Generic to map A
to R , and a CsvEncoder for R , create a CsvEncoder for A .
 */
implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic[A] {type Repr = R},
                                   enc: CsvEncoder[R]
                                 ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

writeCsv(iceCreams)