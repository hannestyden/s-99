import scala.annotation.tailrec

object P11 {
  val name = "P11 - Modified run-length encoding of a list"

  val inputsAndExpectedOutputs = Seq(
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) -> List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)),

    List()               -> List(),
    List('a)             -> List('a),
    List('a, 'a, 'a, 'a) -> List((4, 'a)),
    List('a, 'b, 'a)     -> List('a, 'b, 'a)
  )

  // Naïve
  def encodeModifiedRec[A](list: List[A]): List[Any] = {
    def _encodeModified[A](list: List[A], prev: List[A]): List[List[A]] = {
      (list, prev) match {
        case (Nil, prev)          => List(prev)
        case (head :: tail, Nil)  => _encodeModified(tail, List(head))
        case (head :: tail, prev) => {
          if (head == prev.head) _encodeModified(tail, prev ++ List(head))
          else List(prev) ++ _encodeModified(tail, List(head))
        }
      }
    }

    def elementOrRunLength[T](n: Int, s: A): Any = n match {
      case 1 => s
      case _ => (n, s)
    }

    _encodeModified(list, List()).filter(_.nonEmpty).map(l => (l.length, l.head)).map((elementOrRunLength _).tupled)
  }

  // Tail call optimized
//def encodeTCO[A](list: List[A]): List[List[A]] =

  // Functional
//def encodeFun[A](list: List[A]): List[List[A]] =

  def main(args: Array[String]) = {
    println(name)

    (Seq(
      ("Rec", encodeModifiedRec _)
//  , ("TCO", encodeModifiedTCO _)
//  , ("Fun", encodeModifiedFun _)
    ): Seq[(String, List[Symbol] => List[Any])]).foreach { case (n, f) =>
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
