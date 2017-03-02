package datastructure

/**
  * Created by lhendra15 on 2/6/17.
  */

case class TrieNode(char: Char, var nextNodes: Set[TrieNode])
case class Trie() {

  var root = TrieNode('\n', Set())

  private def buildTrieNode(input: String): TrieNode = {

    def foo(chars: List[Char]): Option[TrieNode] = {
      chars.headOption match {
        case None => None
        case Some(head) =>
          foo(chars.tail) match {
            case None => Some(TrieNode(head, Set()))
            case Some(next) => Some(TrieNode(head, Set(next)))
          }
      }
    }

    require(input.size > 0)
    val inputChars = input.toCharArray.toList
    foo(inputChars).get
  }

  def add(input: String): Unit = {

    val node = buildTrieNode(input)

    def foo(currentNode: TrieNode, nodeToInsert: TrieNode): Unit = {
      currentNode.nextNodes.find(_.char == nodeToInsert.char) match {
        case None =>
          currentNode.nextNodes = currentNode.nextNodes + nodeToInsert
        case Some(foundNode) =>
          if (nodeToInsert.nextNodes.nonEmpty) {
            foo(foundNode, nodeToInsert.nextNodes.head)
          }
      }
    }

    foo(root, node)
  }

  def find(str: String) = {
    require(str.size > 0)
    val strChars = str.toCharArray.toList

    def findStartingNode(node: TrieNode, chars: List[Char]): Option[TrieNode] = {
      chars.headOption match {
        case None => Some(node)
        case Some(head) =>
          node.nextNodes.find(_.char == head) match {
            case None => None
            case Some(nextNode) => findStartingNode(nextNode, chars.tail)
          }
      }
    }

    def bar(prevChars: List[Char], node: TrieNode): Set[List[Char]] = {
      if (node.nextNodes.isEmpty) {
        Set(prevChars)
      } else {
        node.nextNodes.flatMap(n => {
          val thisChain: List[Char] = prevChars ::: List(n.char)
          bar(thisChain, n)
        })
      }
    }

    findStartingNode(root, strChars) match {
      case None => List()
      case Some(startingNode) =>
        bar(strChars, startingNode)
    }

  }

}


object Contact extends App {

  val contactTrie = Trie()
  contactTrie.add("tree")
  contactTrie.add("trie")
  contactTrie.add("also")
  contactTrie.add("all")
  contactTrie.add("algo")
  contactTrie.add("algae")
  contactTrie.add("assoc")

  println(contactTrie.find("tr"))
  println(contactTrie.find("alg"))
  println(contactTrie.find("a"))
  println(contactTrie.find("as"))
}
