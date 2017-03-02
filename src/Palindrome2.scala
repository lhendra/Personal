/**
  * Created by lhendra15 on 1/22/17.
  */
class Palindrome2[T] {

  def isMirror(list: List[T]): Boolean = {
    (list.headOption, list.lastOption) match {
      case (None, None) => //nothing in the list - stop here
        true
      case (Some(head), None) => //only one element in the list - stop here
        true
      case (None, Some(last)) => // this is weird, should not have happened!
        false
      case (Some(head), Some(last)) =>
        if (head != last) {
          false
        } else {
          isMirror(list.tail.dropRight(1))
        }
    }
  }

  def isPalindrome(list: List[T]): Boolean = isMirror(list)

}

object Palindrome2 extends App {
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)))
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 0, 4, 3, 2, 1)))
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 5, 4, 3, 2, 0)))
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)))
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 0)))
  println(new Palindrome2[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 0, 4, 3, 2, 1)))
}
