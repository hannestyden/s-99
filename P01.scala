object P01 {
  val name = "P01"

  def example = last(List(1, 1, 2, 3, 5, 8))
  def expectedValue = 8

  private def last[A](list: List[A]): A = list match {
    case head :: Nil => head
    case _ :: tail   => last(tail)
    case _           => ???
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
