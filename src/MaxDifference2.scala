object MaxDifference2 extends App {

  def maxDifference(a: Array[Int]): Int = {
    val diffs: Array[Int] = getDiffs(a.tail, a.head)
    diffs.toList.sorted.last
  }


  def getDiffs(a: Array[Int], b: Int): Array[Int] = {
    a.toList match {
      case Nil => Array()
      case _ =>
        val currentArray = for {
          value <- a
        } yield {
          value - b
        }

        currentArray ++ getDiffs(a.tail, a.head)
    }
  }


  println(maxDifference(Array(2, 3, 10, 2, 4, 8, 1)))

}
