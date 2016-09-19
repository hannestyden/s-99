import scala.annotation.tailrec

object P08 {
  val name = "P08 - Eliminate consecutive duplicates"

  val inputsAndExpectedOutputs = Seq(
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) -> List('a, 'b, 'c, 'a, 'd, 'e),

    List()               -> List(),
    List('a)             -> List('a),
    List('a, 'a, 'a, 'a) -> List('a),
    List('a, 'b, 'a)     -> List('a, 'b, 'a)
  )

  // Naïve
  def compress[A](list: List[A]): List[A] = {
    def _compress[A](list: List[A], prev: Option[A]): List[A] = {
      list match {
        case Nil => Nil
        case head :: tail => {
          if (Some(head) == prev) _compress(tail, Some(head))
          else head :: _compress(tail, Some(head))
        }
      }
    }

    _compress(list, None)
  }

  // @tailrec
  // def compressTCO[A](list: List[A], prev: Option[A] = None): List[A]

  // HOF
  // def compressHOF[A](list: List[A], prev: Option[A] = None): List[A]

  def main(args: Array[String]) = {
    println(name)

    (Seq(
      ("Recursive", compress _)
    ): Seq[(String, List[Symbol] => List[Symbol])]).foreach { case (n, f) =>
      println(n)
      inputsAndExpectedOutputs.foreach { case (input: List[Symbol], output: List[Symbol]) => {
        println("---")
        report(input, f(input), output)
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
