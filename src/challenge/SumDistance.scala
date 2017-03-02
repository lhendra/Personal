package challenge

/**
  * Created by lhendra15 on 2/7/17.
  */
object SumDistance extends App {

  private def getIndexPairs(indexSize: Int) = {

    def accumPairs(accum: List[(Int, Int)], list: List[Int]): List[(Int, Int)] = {
      val list1 = list
      val list2 = list

      list1.headOption match {
        case None => accum
        case Some(head) =>
          val currentResult = list2.foldLeft(List[(Int, Int)]())((accum, elem) => {
            accum ::: List((head, elem))
          })
          accumPairs(accum ::: currentResult, list.tail)
      }
    }

    val indexList = (0 to indexSize-1 by 1).toList
    accumPairs(List(), indexList)
  }

  def solution2(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    var maxSum = Int.MinValue

    def calculate(indexPair: (Int, Int)): Unit = {
      val result = a(indexPair._1) + a(indexPair._2) + (indexPair._2 - indexPair._1)
      println(s"Result for (${indexPair._1}, ${indexPair._2}): $result")
      if (result > maxSum) {
        maxSum = result
      }
    }


    val indexPairs = getIndexPairs(a.size)
    indexPairs.foreach(calculate(_))
    maxSum
  }

  def solution(a: Array[Int]): Int = {

    var maxSum = Int.MinValue

    def calculate(indexPair: (Int, Int)): Unit = {
      val result = a(indexPair._1) + a(indexPair._2) + (indexPair._2 - indexPair._1)
      println(s"Result for (${indexPair._1}, ${indexPair._2}): $result")
      if (result > maxSum) {
        maxSum = result
      }
    }

    def getPairsAndCalculate(list: List[Int]): Unit = {
      val list1 = list
      val list2 = list

      list1.headOption match {
        case None => // do nothing
        case Some(list1Head) =>
          list2.foreach(l2 => {
            calculate((list1Head, l2))
          })
          getPairsAndCalculate(list.tail)
      }
    }

    val indexList = (0 to a.size-1 by 1).toList
    getPairsAndCalculate(indexList)
    maxSum
  }

  println(s"Max sum-distance: ${solution(Array(1, 3, -3))}")
  println(s"Max sum-distance: ${solution(Array(-8, 4, 0, 5, -3, 6))}")

}
