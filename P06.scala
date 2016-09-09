object P06 {
  val name = "P06 - Palindrome"

  val input  = List(1, 2, 3, 2, 1)
  val output = true

  def isPalindrome[A](list: List[A]): Boolean =
    list == reverseRec(list)

  def reverseRec[A](list: List[A]): List[A] = list match {
    case head :: Nil  => List(head)
    case head :: tail => reverseRec(tail) ++ List(head)
    case Nil          => ???
  }

  def main(args: Array[String]) = {
    println(name)

    println(s"Input: $input")

    compare(isPalindrome(input), output)
  }

  private def compare[A](a: A, b: A) = {
    println(s"Actual:   $a")
    println(s"Expected: $b")
    println(s"Equal:    ${a == b}")
  }
}
