package challenge

/**
  * Created by lhendra15 on 2/7/17.
  */
object DecimalZip extends App {

  private def zip[T](list1: List[T], list2: List[T]) = {

    def combine[T](accum: List[T], list1: List[T], list2: List[T]): List[T] = {
      (list1.headOption, list2.headOption) match {
        case (None, None) => accum
        case (None, Some(e2)) =>
          val newAccum = accum ::: List(e2)
          combine(newAccum, list1, list2.tail)
        case (Some(e1), None) =>
          val newAccum = accum ::: List(e1)
          combine(newAccum, list1.tail, list2)
        case (Some(e1), Some(e2)) =>
          val newAccum = accum ::: List(e1) ::: List(e2)
          combine(newAccum, list1.tail, list2.tail)
      }
    }

    combine(List(), list1, list2)
  }

  def solution(a: Int, b: Int): Int = {
    // write your code in Scala 2.12

    require(a >= 0 && a <= 100000000)
    require(b >= 0 && b <= 100000000)

    val aList = a.toString.toCharArray.toList
    val bList = b.toString.toCharArray.toList
    val aZipBList = zip[Char](aList, bList)
    try {
      val result = aZipBList.mkString.toInt
      if (result > 100000000) {
        -1
      } else {
        result
      }
    } catch {
      case nfe: NumberFormatException => -1
    }
  }

  println(solution(12, 56))
  println(solution(12345, 678))

}

