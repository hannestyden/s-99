object P04 {
  val name = "P04"

  def example = length(List(1, 1, 2, 3, 5, 8))
  def expectedValue = 6

  private def length[A](list: List[A], base: Int = 0): Int = list match {
    case Nil       => base
    case _ :: tail => length(tail, base + 1)
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
