import scala.io.Source

object ParserCsv {
  def parseCsv[A](input: Source)(implicit da: RowDecoder[A]): Iterator[A] =
    input.getLines.map(r => da.decode(r.split(",")))
}

trait CsvInput[T] {
  def asSource(t: T): Source
  def parseCsv[A](t: T)(implicit da: RowDecoder[A]): Iterator[A] =
    asSource(t).getLines.map(row => da.decode(row.split(",")))
}

object CsvInput {
  implicit val stringInput: CsvInput[String] = new CsvInput[String] {
    override def asSource(s: String) = Source.fromString(s)
  }
}