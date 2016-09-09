object P07 {
  val name = "P07 - Flatten list"

  val input  = List(List(1, 1), 2, List(3, List(5, 8)))
  val output = List(1, 1, 2, 3, 5, 8)

  def flatten[A](list: List[A]): List[A] = list match {
    case Nil                     => Nil
    case (head: List[A]) :: tail => flatten(head) ++ flatten(tail)
    case head            :: tail => head          :: flatten(tail)
  }

  def main(args: Array[String]) = {
    println(name)

    println(s"Input:    $input")

    compare(flatten(input), output)
  }

  private def compare[A](a: A, b: A) = {
    println(s"Actual:   $a")
    println(s"Expected: $b")
    println(s"Equal:    ${a == b}")
  }
}
