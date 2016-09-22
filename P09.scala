import scala.annotation.tailrec

object P09 {
  val name = "P09 - Pack consecutive duplicates of list elements into sublists"

  val inputsAndExpectedOutputs = Seq(
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) -> List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)),

    List()               -> List(List()),
    List('a)             -> List(List('a)),
    List('a, 'a, 'a, 'a) -> List(List('a, 'a, 'a, 'a)),
    List('a, 'b, 'a)     -> List(List('a), List('b), List('a))
  )

  // Naïve
  def packRec[A](list: List[A]): List[List[A]] = {
    def _pack[A](list: List[A], prev: List[A]): List[List[A]] = {
      (list, prev) match {
        case (Nil, prev)          => List(prev)
        case (head :: tail, Nil)  => _pack(tail, List(head))
        case (head :: tail, prev) => {
          if (head == prev.head) _pack(tail, prev ++ List(head))
          else List(prev) ++ _pack(tail, List(head))
        }
      }
    }

    _pack(list, List())
  }

  // Tail call optimized
//def packTCO[A](list: List[A]): List[List[A]] =

  // Functional
//def packFun[A](list: List[A]): List[List[A]] =

  def main(args: Array[String]) = {
    println(name)

    (Seq(
      ("Rec", packRec _)
//  , ("TCO", packTCO _)
//  , ("Fun", packFun _)
    ): Seq[(String, List[Symbol] => List[List[Symbol]])]).foreach { case (n, f) =>
      println(n)
      inputsAndExpectedOutputs.foreach { case (input, expectedOutput) => {
        println("---")
        report(input, f(input), expectedOutput)
      }}
    }

    println("---")
  }

  private def report[A](i: A, a: A, e: A) = {
    println(s"Input:           $i")
    println(s"Actual output:   $a")
    println(s"Expected output: $e")
    println(s"Equal:           ${a == e}")
  }
}
