package level2

import shapeless.LabelledGeneric

object Example4 extends App {
  val gen = LabelledGeneric[Record]
  println(gen)
}
