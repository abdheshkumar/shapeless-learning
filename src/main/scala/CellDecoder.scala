

trait CellDecoder[T] {
  def decode(s: String): T
}

object CellDecoder {
  def instance[A](f: String => A): CellDecoder[A] = new CellDecoder[A] {
    override def decode(s: String): A = f(s)
  }

  implicit val intDecoder: CellDecoder[Int] = instance(_.toInt)
  implicit val stringDecoder: CellDecoder[String] = instance(identity)
  implicit val booleanDecoder: CellDecoder[Boolean] = instance(_.toBoolean)

  implicit def eitherCellDecoder[A, B](implicit da: CellDecoder[A], db: CellDecoder[B]): CellDecoder[Either[A, B]] =
    instance { s =>
      try {
        Right(db.decode(s))
      }
      catch {
        case _: Throwable => Left(da.decode(s))
      }
    }

  implicit def optionCellDecoder[A](implicit da: CellDecoder[A]): CellDecoder[Option[A]] =
    instance(s => if (s.isEmpty) None else Some(da.decode(s)))

  import shapeless._

  implicit val hnilFlip: CellDecoder[HNil] = instance(_ => HNil)

 /* implicit def hconsFlip[H, T <: HList](implicit
                                        hf: CellDecoder[H],
                                        tf: CellDecoder[T]): CellDecoder[H :: T] = new CellDecoder[H :: T] {
    override def apply(x: H :: T): H :: T = hf(x.head) :: tf(x.tail)
  }*/

  // summon implicit instances, tf is computed recursively

}
