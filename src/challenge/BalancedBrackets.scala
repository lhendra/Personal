package challenge

import scala.collection.immutable.HashSet
import scala.collection.mutable.Stack.StackBuilder

case class BracketPair(left: Char, right: Char)

case class BalancedBrackets() {

  val validBrackets: HashSet[BracketPair] = HashSet(BracketPair('[',']'), BracketPair('{','}'), BracketPair('(',')'))
  val bracketStack: StackBuilder[Char] = new StackBuilder()

  def eval(input: String): Boolean = {

    def isBalanced(list: List[Char]): Boolean = {
      list.size match {
        case 0 => true
        case 1 => false
        case _ =>
          val foundPair = BracketPair(list.head, list.last)
          if (validBrackets.contains(foundPair)) {
            isBalanced(list.tail.dropRight(1))
          } else {
            false
          }
      }
    }
    val list = input.toCharArray.toList
    isBalanced(list)
  }

}

object BalancedBrackets extends App {

  println(BalancedBrackets().eval("{[()]}") == true)
  println(BalancedBrackets().eval("{[(])}") == false)
  println(BalancedBrackets().eval("{{[[(())]]}}") == true)

}

