import scala.annotation.tailrec

object P10 {
  val name = "P10 - Run-length encoding of a list."

  val inputsAndExpectedOutputs = Seq(
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) -> List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)),

    List()               -> List(),
    List('a)             -> List((1, 'a)),
    List('a, 'a, 'a, 'a) -> List((4, 'a)),
    List('a, 'b, 'a)     -> List((1, 'a), (1, 'b), (1, 'a))
  )

  // Naïve
  def encodeRec[A](list: List[A]): List[(Int, A)] = {
    def _encode[A](list: List[A], prev: List[A]): List[List[A]] = {
      (list, prev) match {
        case (Nil, prev)          => List(prev)
        case (head :: tail, Nil)  => _encode(tail, List(head))
        case (head :: tail, prev) => {
          if (head == prev.head) _encode(tail, prev ++ List(head))
          else List(prev) ++ _encode(tail, List(head))
        }
      }
    }

    _encode(list, List()).filter(_.nonEmpty).map(l => (l.length, l.head))
  }

  // Tail call optimized
//def encodeTCO[A](list: List[A]): List[List[A]] =

  // Functional
//def encodeFun[A](list: List[A]): List[List[A]] =

  def main(args: Array[String]) = {
    println(name)

    (Seq(
      ("Rec", encodeRec _)
//  , ("TCO", encodeTCO _)
//  , ("Fun", encodeFun _)
    ): Seq[(String, List[Symbol] => List[(Int, Symbol)])]).foreach { case (n, f) =>
      println(n)
      inputsAndExpectedOutputs.foreach { case (input, expectedOutput) => {
        println("---")
        report(input, f(input), expectedOutput)
      }}
    }

    println("---")
  }

  private def report[A](i: A, a: A, e: A) = {
    println(s"Input:  $i")
    println(s"A eq E: ${a == e}")
    if (a != e) {
      println(s"Actual output:   $a")
      println(s"Expected output: $e")
    }
  }
}
