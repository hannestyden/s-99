object P02 {
  val name = "P02"

  def example = penultimate(List(1, 1, 2, 3, 5, 8))
  def expectedValue = 5

  private def penultimate[T](list: List[T]): T = list match {
    case Nil                     => ???
    case penultimate :: _ :: Nil => penultimate
    case head :: tail            => penultimate(tail)
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
