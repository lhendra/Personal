object MaxDifference3 extends App {
  def maxDiff(input: Array[Int]): Int = {
    require(input.size >= 2)
    val sortedInput = input.sortWith(_ < _)
    sortedInput.last - sortedInput.head
  }

  println(maxDiff(Array(2, 3, 10, 2, 4, 8, 1)))
}
