package challenge

object SmallestRangeAllLists extends App {

  def zoo(valueToCompare: Int, list: List[Int]): (Int, List[Int]) = {
    val valueWithMinDiff = list.minBy(value => Math.abs(value - valueToCompare))
    val remaining = list.filter(_ >= valueWithMinDiff)
    (valueWithMinDiff, remaining)
  }

  def bar(valueToCompare: Int, listsToSearch: List[List[Int]]): List[(Int, List[Int])] = {
    for {
      list <- listsToSearch
    } yield {
      zoo(valueToCompare, list)
    }
  }

  def findSmallestRange(lists: List[List[Int]]): (Int, Int) = {


    def foo(smallestRangeSoFar: (Int, Int), listToCompare: List[Int], listsToSearch: List[List[Int]]): (Int, Int) = {
      listToCompare.headOption match {
        case None => smallestRangeSoFar
        case Some(headValue) =>
          val currentResult = bar(headValue, listsToSearch)
          val currentRange = (headValue :: currentResult.map(_._1)).sorted
          val remaining = currentResult.map(_._2)
          if ((currentRange.last - currentRange.head) < (smallestRangeSoFar._2 - smallestRangeSoFar._1)) {
            foo((currentRange.head, currentRange.last), listToCompare.tail, remaining)
          } else {
            foo(smallestRangeSoFar, listToCompare.tail, remaining)
          }
      }

    }

    foo((0, Int.MaxValue), lists.head, lists.tail)
  }

  val input = List(List(4, 10, 15, 24, 26), List(0, 9, 12, 20), List(5, 18, 22, 30))
  println(findSmallestRange(input) == (20, 24))

//  println(zoo(4, List(0, 9, 12, 20)))
//  println(zoo(8, List(0, 9, 12, 20)))
//
//  println(bar(4, List(List(0, 9, 12, 20), List(5, 18, 22, 30))))
//  println(bar(10, List(List(0, 9, 12, 20), List(5, 18, 22, 30))))
//  println(bar(15, List(List(0, 9, 12, 20), List(5, 18, 22, 30))))

}
