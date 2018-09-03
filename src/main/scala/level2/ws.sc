import level2.Record
import shapeless.{Generic, LabelledGeneric}

//https://www.lyh.me/automatic-type-class-derivation-with-shapeless.html#.W10HyXYzqrd

val labelGen = LabelledGeneric[Record]

/*
gen: shapeless.LabelledGeneric[level2.Record]{type Repr =
Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("i")],Int]
:: Double with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("d")],Double]
:: String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("s")],String]
:: Boolean with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("b")],Boolean]
:: shapeless.HNil} = shapeless.LabelledGeneric$$anon$1@6039ffc
* */

case class Record(i: Int, d: Double, s: String, b: Boolean)
// Record <=> Int :: Double :: String :: Boolean :: HNil
val gen = Generic[Record]
val l = gen.to(Record(1, 3.14, "foo", true)) // 1 :: 3.14 :: "foo" :: true :: HNil
val g = gen.from(l) // Record(1, 3.14, "foo", true)


//Type aliases are also called type members