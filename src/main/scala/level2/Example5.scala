package level2

object Example5 extends App {

  import shapeless._
  import shapeless.labelled.FieldType

  trait ToMap[T] {
    def apply(x: T): Map[String, Any]
  }



  implicit val hnilToMap = new ToMap[HNil] {
    override def apply(x: HNil): Map[String, Any] = Map.empty
  }

  implicit def hconsToMap[K <: Symbol, H, T <: HList]
  (implicit wit: Witness.Aux[K], ttm: ToMap[T])
  : ToMap[FieldType[K, H] :: T] = new ToMap[FieldType[K, H] :: T] {
    override def apply(x: FieldType[K, H] :: T): Map[String, Any] =
      ttm(x.tail) + (wit.value.name -> x.head)
  }

  def toMap[T, L <: HList](x: T)
                          (implicit
                           gen: LabelledGeneric.Aux[T, L],
                           tm: ToMap[L]): Map[String, Any] =
    tm(gen.to(x))

  case class Record(i: Int, d: Double, s: String, b: Boolean)

  //toMap(Record(1, 3.14, "foo", true)) // Map("b" -> true, "s" -> "foo", d -> 3.14, i -> 1)
}

