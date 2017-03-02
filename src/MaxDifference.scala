object MaxDifference extends App {

  def maxDifference(a: Array[Int]): Int = {
    maxDiffTemp(a.tail, a.head)
  }


  def maxDiffTemp(a: Array[Int], b: Int): Int = {
    a.toList match {
      case Nil => 0
      case _ =>
        val foo: Array[Int] = for {
          value <- a
        } yield {
          value - b
        }
        val currentMaxDiff = foo.toList.sorted.last
        val nextMaxDiff = maxDiffTemp(a.tail, a.head)
        if (currentMaxDiff > nextMaxDiff) {
          currentMaxDiff
        } else {
          nextMaxDiff
        }
    }
  }


  println(maxDifference(Array(2, 3, 10, 2, 4, 8, 1)))

}
