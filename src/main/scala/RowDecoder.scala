trait RowDecoder[T] {
  def decode(row: IndexedSeq[String]): T
}

object RowDecoder {
  implicit def tupleOne[A](implicit da: CellDecoder[A]): RowDecoder[A] = new RowDecoder[A] {
    override def decode(row: IndexedSeq[String]): A = da.decode(row(0))
  }

  implicit def tupleTree[A, B, C](
                                   implicit da: CellDecoder[A],
                                   db: CellDecoder[B],
                                   dc: CellDecoder[C]): RowDecoder[(A, B, C)] = new RowDecoder[(A, B, C)] {
    override def decode(row: IndexedSeq[String]): (A, B, C) = (da.decode(row(0)), db.decode(row(1)), dc.decode(row(2)))
  }

  //Write similar instances for 22-tuples


  import scala.collection.generic.CanBuildFrom

  implicit def collectionDecoder[A, C[_]](implicit da: CellDecoder[A],
                                          cbf: CanBuildFrom[Nothing, A, C[A]]): RowDecoder[C[A]] =
    new RowDecoder[C[A]] {
      override def decode(ss: IndexedSeq[String]) = ss.foldLeft(cbf.apply())((out, s) => out += da.decode(s)).result()
    }

  def caseClass2Decoder[A0, A1, C](f: (A0, A1) => C)(i0: Int, i1: Int)(implicit d0: CellDecoder[A0], d1: CellDecoder[A1]): RowDecoder[C] =
    new RowDecoder[C] {
      override def decode(row: IndexedSeq[String]) = f(d0.decode(row(i0)), d1.decode(row(i1)))
    }

  implicit def tuple2Decoder[A0, A1](implicit d0: CellDecoder[A0], d1: CellDecoder[A1]): RowDecoder[(A0, A1)] =
    caseClass2Decoder(Tuple2.apply[A0, A1])(0, 1)
}
