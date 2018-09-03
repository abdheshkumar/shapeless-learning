object CSVApp extends App {
  val result = implicitly[CsvInput[String]].parseCsv[(Int, Int)]("1,2\n3,4").toList
  println(result)
}
