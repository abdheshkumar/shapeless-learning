import shapeless.{::, Generic, HList, HNil}
import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

val aa: ::[String, ::[Int, ::[Boolean, HNil]]] = "Sunday" :: 1 :: false :: HNil

case class User[A, B](name: A, age: B)

val user: User[String, User[Int, Int]] = User("dsd", User(1, 2))

//val user1: String User Int User Int = User("dsd", User(1, 2))
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val iceCreamGen = Generic[IceCream]
val iceCream = IceCream("Sundae", 1, false)
val repr = iceCreamGen.to(iceCream)
val iceCream2 = iceCreamGen.from(repr)

case class Employee(name: String, number: Int, manager: Boolean)

val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

val tupleGen = Generic[(String, Int, Boolean)]
tupleGen.to(("Hello", 123, true))
tupleGen.from("Hello" :: 123 :: true :: HNil)


case class Red()

case class Amber()

case class Green()

type Light = Red :+: Amber :+: Green :+: CNil

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape
case object Triangle extends Shape

val gen = Generic[Shape]
gen.to(Rectangle(3.0, 4.0))
// res3: gen.Repr = Inl(Rectangle(3.0,4.0))
gen.to(Circle(1.0))
val t = gen.to(Triangle)

