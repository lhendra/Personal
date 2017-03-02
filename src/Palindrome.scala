/**
  * Created by v769217 on 2/27/16.
  */
class Palindrome[T]  {

  def isPalindrome(list: List[T]): Boolean = {
    if ((list.size % 2) == 0) {
      val temp = list.splitAt(list.size/2)
      isMirror(temp._1, temp._2)
    } else {
      val halfway = list.size / 2
      isMirror(list.take(halfway), list.drop(halfway+1))
    }
  }

  def isMirror(list1: List[T], list2: List[T]): Boolean = {
    require(list1.size == list2.size)

    list1 match {
      case Nil => true
      case _ =>
        if (list1.head == list2.last) {
          isMirror(list1.tail, list2.dropRight(1))
        } else {
          false
        }
    }

  }


}

object Palindrome extends App {
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)))
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 0, 4, 3, 2, 1)))
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 5, 4, 3, 2, 0)))
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)))
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 0)))
  println(new Palindrome[Int].isPalindrome(List(1, 2, 3, 4, 5, 6, 0, 4, 3, 2, 1)))
}