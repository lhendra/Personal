package datastructure

import scala.collection.mutable.ListBuffer

/**
  * Created by lhendra15 on 2/14/17.
  */
case class MaxHeap[T]()(implicit ordering:Ordering[T]) {

  var elems: List[T] = List()


  private def maxHeapifyUpwards(buffer: ListBuffer[T], index: Int): ListBuffer[T] = {

    if (index <= 1) { // we stop at root
      buffer
    } else {
      val parentIndex = index / 2
      if (ordering.lt(buffer(parentIndex), buffer(index))) {
        val temp = buffer(index)
        buffer(index) = buffer(parentIndex)
        buffer(parentIndex) = temp
      }
      maxHeapifyUpwards(buffer, parentIndex)
    }

  }

  private def maxHeapifyDownwards(buffer: ListBuffer[T], index: Int): ListBuffer[T] = {
    val childIndexLeft = index*2
    val childIndexRight = (index*2)+1
    val childIndexToUse = Math.min(childIndexLeft, childIndexRight)
    if (childIndexToUse >= buffer.size) { // we stop at last element in index
      buffer
    } else {
      if (ordering.gt(buffer(childIndexToUse), buffer(index))) {
        val temp = buffer(index)
        buffer(index) = buffer(childIndexToUse)
        buffer(childIndexToUse) = temp
      }
      maxHeapifyDownwards(buffer, childIndexToUse)
    }

  }

  def push(newElem: T) = {
    val buffer: ListBuffer[T] = ListBuffer(newElem)
    buffer.append(elems: _*)
    buffer.append(newElem)
    elems = maxHeapifyUpwards(buffer, buffer.size-1).tail.toList
  }

  def pop(): T = {
    val max = elems.head

    if (elems.size == 1) {
      elems = List()
    } else {
      val newElems = elems.last :: elems.tail.dropRight(1)
      val buffer: ListBuffer[T] = ListBuffer(elems.last)
      buffer.append(newElems: _*)
      elems = maxHeapifyDownwards(buffer, 1).tail.toList
    }

    max
  }

  def pushAll(newElems: List[T]) = {
    newElems.foreach(push(_))
  }

  def popTop(count: Int): List[T] = {
    (1 to count by 1).map(_ => pop).toList
  }

}

object MaxHeap extends App {

  val maxHeap = MaxHeap[Int]()
  maxHeap.pushAll(List(4, 8, 1, 7, 3))
  println(maxHeap.popTop(5) == List(8, 7, 4, 3, 1))

}
