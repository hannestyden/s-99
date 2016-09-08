object P01 {
  val name = "P01"

  def example = last(List(1, 1, 2, 3, 5, 8))
  def expectedValue = 8

  private def last[T](list: List[T]): T = list match {
    case Nil          => ???
    case last :: Nil  => last
    case head :: tail => last(tail)
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
