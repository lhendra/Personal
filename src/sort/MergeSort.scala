package sort

/**
  * Created by lhendra15 on 1/22/17.
  */
case class MergeSort[T]()(implicit ordering:Ordering[T]) {

  def sort(list: List[T]): List[T] = {
    if (list.size <= 1) {
      list
    } else {
      val (left, right) = list.splitAt(list.size/2)
      val sortedLeft = sort(left)
      val sortedRight = sort(right)
      merge(sortedLeft, sortedRight)
    }
  }

  private def merge(left: List[T], right: List[T]): List[T] = {

    def sortAndMerge(left: List[T], right: List[T]): List[T] = {
      (left.headOption, right.headOption) match {
        case (Some(leftElem), Some(rightElem)) =>
          if (ordering.lt(leftElem, rightElem)) {
            leftElem :: sortAndMerge(left.tail, right)
          } else {
            rightElem :: sortAndMerge(left, right.tail)
          }
        case (Some(leftElem), None) => left
        case (None, Some(rightElem)) => right
        case (None, None) => List()
      }
    }

    sortAndMerge(left, right)

  }

}

object MergeSort extends App {

  println(MergeSort[Int]().sort(List(8, 3, 1, 6, 4, 7, 10, 14, 13)) == List(1, 3, 4, 6, 7, 8, 10, 13, 14))
  println(MergeSort[Int]().sort(List(8, 3, 1, 6, 4, 4, 7, 10, 14, 13)) == List(1, 3, 4, 4, 6, 7, 8, 10, 13, 14))
}