package challenge

import scala.util.Random

object Equilibrium extends App {

  def solution(a: Array[Int]): Int = {

    var equilibriumIndexes: List[Int] = List()

    val aList = a.toList

    // write your code in Scala 2.12
    (0 to a.size-1 by 1).toList.foreach(index => {
      val left = aList.take(index)
      val right = aList.takeRight(a.size - index).tail
      println(s"$left ::: $right")
      if (left.sum == right.sum) {
        equilibriumIndexes = index :: equilibriumIndexes
      }
    })

    if (equilibriumIndexes.isEmpty) {
      -1
    } else {
      equilibriumIndexes(Random.nextInt(equilibriumIndexes.size))
    }

  }

  val input = Array(-1, 3, -4, 5, 1, -6, 2, 1)
  println(solution(input))
  println(solution(input))
  println(solution(input))
  println(solution(input))
  println(solution(input))
  println(solution(input))
  println(solution(input))

}