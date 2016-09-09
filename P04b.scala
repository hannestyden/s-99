object P04b {
  val name = "P04"

  def example = length(List(1, 1, 2, 3, 5, 8))
  def expectedValue = 6

  private def length[A](list: List[A]): Int = list match {
    case Nil       => 0
    case _ :: tail => 1 + length(tail)
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
