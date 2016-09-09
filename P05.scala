import scala.annotation.tailrec

object P05 {
  val name = "P05 - Reverse list"

  val input  = List(1, 1, 2, 3, 5, 8)
  val output = List(8, 5, 3, 2, 1, 1)

  def reverseRec[A](list: List[A]): List[A] = list match {
    case head :: Nil  => List(head)
    case head :: tail => reverseRec(tail) ++ List(head)
    case Nil          => ???
  }

  @tailrec
  def reverseTCO[A](list: List[A], base: List[A] = List()): List[A] = list match {
    case head :: Nil  => List(head) ++ base
    case head :: tail => reverseTCO(tail, List(head) ++ base)
    case Nil          => ???
  }

  def main(args: Array[String]) = {
    println(name)

    println(s"Input: $input")

    println("Recursive")
    compare(reverseRec(input), output)

    println("Tail call optimized")
    compare(reverseTCO(input), output)
  }

  private def compare[A](a: A, b: A) = {
    println(s"Actual:   $a")
    println(s"Expected: $b")
    println(s"Equal:    ${a == b}")
  }
}
