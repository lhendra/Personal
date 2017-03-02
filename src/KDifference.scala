

object KDifference extends App {

  def KDifference(a: Array[Int], k: Int): Int = {
    difference(a, k, 0)
  }

  def difference(a: Array[Int], k: Int, countSoFar: Int): Int = {
    a match {
      case Array() => countSoFar
      case _ =>
        val newCount = traverseAndCheckKDiff(a.head, a.tail, k)
        difference(a.tail, k, countSoFar+newCount)
    }
  }

  def traverseAndCheckKDiff(x: Int, y: Array[Int], k: Int): Int = {
    def diffCount(x1: Int, x2: Int): Int = {
      val diff = (Math.abs(x1 - x2))
      if (diff == k) {
        1
      } else {
        0
      }
    }

    y match {
      case Array() => 0
      case _ =>
        val countArray: Array[Int] = for {
          value <- y
        } yield {
          diffCount(value, x)
        }
        countArray.reduceLeft[Int](_+_)
    }
  }

  println(KDifference(Array(1, 5, 3, 4, 2), 2))

}