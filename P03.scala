object P03 {
  val name = "P03"

  def example = nth(2, List(1, 1, 2, 3, 5, 8))
  def expectedValue = 2

  private def nth[A](index: Int, list: List[A]): A = (index, list) match {
    case (0, head :: _) => head
    case (_, _ :: tail) => nth(index - 1, tail)
    case (_, Nil)       => ???
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
