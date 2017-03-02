/**
  * Created by lhendra15 on 1/22/17.
  */
object ContiguousSequenceLargestSum extends App {

  private def traverse(existing: List[Int], list: List[Int]): List[Int] = {
    list.headOption match {
      case Some(x) if x >= 0 =>
        traverse(existing ::: List(x), list.tail)
      case Some(x) if x < 0 =>
        val temp = getSequenceWithMaxSumBackWard(List.empty, list.tail)
        if (temp.sum + x > 0) {
          existing ::: (x :: temp)
        } else {
          existing
        }
      case None =>
        existing
    }
  }

  def getSequenceWithMaxSumBackWard(listWithMaxSumSoFar: List[Int], list: List[Int]): List[Int] = {
    list match {
      case Nil => listWithMaxSumSoFar
      case _ =>
        if (list.sum >= listWithMaxSumSoFar.sum) {
          getSequenceWithMaxSumBackWard(list, list.dropRight(1))
        } else {
          getSequenceWithMaxSumBackWard(listWithMaxSumSoFar, list.dropRight(1))
        }
    }
  }

  def getSequenceWithMaxSumForward(listWithMaxSumSoFar: List[Int], list: List[Int]): List[Int] = {
    list match {
      case Nil => listWithMaxSumSoFar
      case _ =>
        val temp = traverse(List.empty, list)
        if (temp.sum >= listWithMaxSumSoFar.sum) {
          getSequenceWithMaxSumForward(temp, list.tail)
        } else {
          getSequenceWithMaxSumForward(listWithMaxSumSoFar, list.tail)
        }
    }
  }

  def find(list: List[Int]) = {
    getSequenceWithMaxSumForward(List.empty, list)
  }

  println(find(List(2, 3, -8, -1, 2, 4, -2, 3)))
  println(find(List(5, -9, 6, -2, 3)))
  println(find(List(-3, -10, -5)))

}
