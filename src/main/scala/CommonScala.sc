trait A {
  def test = A.a
}

object A {
  private val a: String = "A"
}
