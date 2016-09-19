import scala.annotation.tailrec

object P08 {
  val name = "P08 - Eliminate consecutive duplicates"

  val io: Seq[(List[Symbol], List[Symbol])] = Seq(
    (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e), List('a, 'b, 'c, 'a, 'd, 'e))
  )

  // Naïve
  def compress[A](list: List[A], prev: Option[A] = None): List[A] = list match {
    case Nil => Nil
    case head :: tail => {
      if (Some(head) == prev) compress(tail, Some(head))
      else                    head :: compress(tail, Some(head))
    }
  }

  // @tailrec
  // def compressTCO[A](list: List[A], prev: Option[A] = None): List[A]

  // HOF
  // def compressHOF[A](list: List[A], prev: Option[A] = None): List[A]

  def main(args: Array[String]) = {
    println(name)

    io.foreach { case (input: List[Symbol], output: List[Symbol]) => {
      println(s"Input:    $input")
      compare(compress(input), output)
    } }
  }

  private def compare[A](a: A, b: A) = {
    println(s"Actual:   $a")
    println(s"Expected: $b")
    println(s"Equal:    ${a == b}")
  }
}
