import scala.annotation.tailrec

object P04 {
  val name = "P04 - Get length of list"

  def input  = List(1, 1, 2, 3, 5, 8)
  def output = 6

  def lengthRec[A](list: List[A]): Int = list match {
    case Nil       => 0
    case _ :: tail => 1 + lengthRec(tail)
  }

  @tailrec
  def lengthTCO[A](list: List[A], base: Int = 0): Int = list match {
    case Nil       => base
    case _ :: tail => lengthTCO(tail, base + 1)
  }

  def lengthFold[A](list: List[A]): Int =
    list.foldLeft(0) { (a, b) => a + 1 }

  def main(args: Array[String]) = {
    println(name)

    println(s"Input: $input")

    println("Recursive")
    compare(lengthRec(input), output)

    println("Tail call optimized")
    compare(lengthTCO(input), output)

    println("Fold")
    compare(lengthFold(input), output)
  }

  private def compare[A](a: A, b: A) = {
    println(s"Actual:   $a")
    println(s"Expected: $b")
    println(s"Equal:    ${a == b}")
  }
}
