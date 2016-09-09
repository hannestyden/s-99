object P05 {
  val name = "P05"

  def example = reverse(List(1, 1, 2, 3, 5, 8))
  def expectedValue = List(8, 5, 3, 2, 1, 1)

  private def reverse[A](list: List[A]): List[A] = list match {
    case head :: Nil  => List(head)
    case head :: tail => reverse(tail) ++ List(head)
    case Nil          => ???
  }

  def main(args: Array[String]) = {
    println(name)

    val actual = example
    println(s"Actual:   $actual")
    println(s"Expected: $expectedValue")
    println(s"Equal:    ${actual == expectedValue}")
  }
}
