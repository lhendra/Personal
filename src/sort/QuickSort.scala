package sort

import scala.collection.mutable

/**
  * Created by lhendra15 on 1/22/17.
  */
case class QuickSort[T]()(implicit ordering:Ordering[T]) {

  def sort(list: List[T]): List[T] = {

    def split(list: List[T], pivot: T): (List[T], List[T]) = {
      val lessList = mutable.ListBuffer[T]()
      val moreList = mutable.ListBuffer[T]()
      list.foreach(elem => {
        if (ordering.lt(elem, pivot)) {
          lessList += elem
        } else {
          moreList += elem
        }
      })
      (lessList.toList, moreList.toList)
    }

    if (list.size <= 1) {
      list
    } else {
      val pivot = list.head
      val (lessList, moreList) = split(list.tail, pivot)
      sort(lessList) ::: (pivot :: sort(moreList))
    }

  }

}

object QuickSort extends App {

  println(QuickSort[Int]().sort(List(8, 3, 1, 6, 4, 7, 10, 14, 13)) == List(1, 3, 4, 6, 7, 8, 10, 13, 14))
  println(QuickSort[Int]().sort(List(8, 3, 1, 6, 4, 4, 7, 10, 14, 13)) == List(1, 3, 4, 4, 6, 7, 8, 10, 13, 14))
}