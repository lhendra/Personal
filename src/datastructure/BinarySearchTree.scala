package datastructure



case class Node[T](var value: T, var right: Option[Node[T]], var left: Option[Node[T]], var parent: Option[Node[T]])
                  (implicit ordering:Ordering[T]){

  def find(valueToFind: T): Option[Node[T]] = {
    valueToFind match {
      case x if ordering.equiv(x, value) => Some(this)
      case x if (ordering.lt(x, value)) =>
        left match {
          case None => None
          case Some(leftNode) => leftNode.find(valueToFind)
        }
      case x if (ordering.gt(x, value)) =>
        right match {
          case None => None
          case Some(rightNode) => rightNode.find(valueToFind)
        }
    }
  }

  def insert(valueToInsert: T): Unit = {
    valueToInsert match {
      case x if ordering.equiv(x, value) =>
        None // value already exists, so return None
      case x if (ordering.lt(x, value))  =>
        left match {
          case None => left = Some(Node(valueToInsert, None, None, Some(this)))
          case Some(leftNode) => leftNode.insert(valueToInsert)
        }
      case x if (ordering.gt(x, value))  =>
        right match {
          case None => right = Some(Node(valueToInsert, None, None, Some(this)))
          case Some(rightNode) => rightNode.insert(valueToInsert)
        }
    }
  }

  private def reAssignParentNode(valueToDelete: T, nodeToReassign: Option[Node[T]]) = {
    parent.map(parentNode => {
      if (ordering.gt(parentNode.value, valueToDelete)) {
        parentNode.left = nodeToReassign
      } else {
        parentNode.right = nodeToReassign
      }
    })
  }

  private def findLeftMostNode(): Node[T] = {
    left match {
      case None => this
      case Some(leftNode) => leftNode.findLeftMostNode
    }
  }

  def delete(valueToDelete: T) = {
    val foundNodeToDelete = find(valueToDelete)
    foundNodeToDelete match {
      case None => // can't find the value to delete - no need to do anything
      case Some(nodeToDelete) =>
        (nodeToDelete.left, nodeToDelete.right) match {
          case (None, None) =>
            // node to delete is a leaf: tell the parentNode to point to None
            reAssignParentNode(valueToDelete, None)
          case (Some(leftNode), None) =>
            // node to delete only has a left node: tell the parentNode to point to the this node's left node
            reAssignParentNode(valueToDelete, Some(leftNode))
          case (None, Some(rightNode)) =>
            // node to delete only has a right node: tell the parentNode to point to the this node's right node
          case (Some(leftNode), Some(rightNode)) =>
            // node to delete has both left and right nodes: use the right node's left most node to replace this node
            val rightNodesLeftMostNode = rightNode.findLeftMostNode
            value = rightNode.value
            rightNodesLeftMostNode.parent.map(parentNode => parentNode.left = rightNodesLeftMostNode.right)
        }
    }
  }

  def traverseInOrder(): List[T] = {
    val leftList = left.map(_.traverseInOrder).getOrElse(List[T]())
    val rightList = right.map(_.traverseInOrder).getOrElse(List[T]())
    leftList ::: (value :: rightList)
  }

  def traversePreOrder(): List[T] = {
    val leftList = left.map(_.traversePreOrder()).getOrElse(List[T]())
    val rightList = right.map(_.traversePreOrder()).getOrElse(List[T]())
    value :: leftList ::: rightList
  }

  def traversePostOrder(): List[T] = {
    val leftList = left.map(_.traversePostOrder()).getOrElse(List[T]())
    val rightList = right.map(_.traversePostOrder()).getOrElse(List[T]())
    leftList ::: rightList ::: List(value)
  }
}

class BinarySearchTree[T]()(implicit ordering:Ordering[T]) {

  var root: Option[Node[T]] = None

  def insert(valueToInsert: T) = {
    root match {
      case None => root = Some(new Node(valueToInsert, None, None, None))
      case Some(rootNode) => rootNode.insert(valueToInsert)
    }
  }

  def find(value: T): Option[Node[T]] = {
    root.flatMap(_.find(value))
  }

  def delete(valueToDelete: T) = {
    root match {
      case None => // do nothing
      case Some(rootNode) =>
        if (rootNode.right == None && rootNode.left == None) {
          if (ordering.equiv(rootNode.value, valueToDelete)) {
            root = None
          } // else, nothing to do
        } else {
          rootNode.delete(valueToDelete)
        }
    }
  }

  def traverseInOrder(): Option[List[T]] = {
    root.map(_.traverseInOrder)
  }

  def traversePreOrder(): Option[List[T]] = {
    root.map(_.traversePreOrder)
  }

  def traversePostOrder(): Option[List[T]] = {
    root.map(_.traversePostOrder)
  }


}

object BinarySearchTree extends App {

  val bst = new BinarySearchTree[Int]
  bst.insert(8)
  bst.insert(3)
  bst.insert(10)
  bst.insert(1)
  bst.insert(6)
  bst.insert(14)
  bst.insert(4)
  bst.insert(7)
  bst.insert(13)

  println(bst.traverseInOrder == Some(List(1, 3, 4, 6, 7, 8, 10, 13, 14)))
  println(bst.traversePreOrder == Some(List(8, 3, 1, 6, 4, 7, 10, 14, 13)))
  println(bst.traversePostOrder == Some(List(1, 4, 7, 6, 3, 13, 14, 10, 8)))

  def isBST(): Boolean = {

    def foo(node: Node[Int], minVal: Int, maxVal: Int): Boolean = {
      if (node.value > minVal) {
        if (node.value < maxVal) {
          node.left match {
            case None => true
            case Some(l) => foo(l, minVal, node.value)
          }
          node.right match {
            case None => true
            case Some(r) => foo(r, node.value, maxVal)
          }
        } else {
          false
        }
      } else {
        false
      }
    }

    bst.root match {
      case None => true
      case Some(r) =>
        (r.left, r.right) match {
          case (None, None) => true
          case (Some(left), Some(right)) =>
            foo(left, Int.MinValue, r.value) && foo(right, r.value, Int.MaxValue)
          case _ => false
        }
    }
  }

  println(isBST)

}
