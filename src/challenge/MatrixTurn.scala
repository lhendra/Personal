package challenge

/**
  * Created by lhendra15 on 2/4/17.
  *
  * 1 2 3
  * 4 5 6
  * 7 8 9
  *   |
  *   |
  * 7 4 1
  * 8 5 2
  * 9 6 3
  *
  */
object MatrixTurn extends App {

  val matrix: List[List[Int]] = List(List(1,2,3), List(4, 5, 6), List(7, 8, 9))

  def turnRight90Degrees(matrix: List[List[Int]]) = {

    def foo(accum: List[List[Int]], rest: List[List[Int]]): List[List[Int]] = {
      if (rest.head.isEmpty) {
        accum
      } else {
        val thisRow: List[Int] = rest.map(_.head).reverse
        val newAccum = accum ::: List(thisRow)
        val nextListOfList = rest.map(_.tail)
        foo(newAccum, nextListOfList)
      }
    }

    foo(List(), matrix)
  }

  val turned1 = turnRight90Degrees(matrix)
  val turned2 = turnRight90Degrees(turned1)
  val turned3 = turnRight90Degrees(turned2)
  val turned4 = turnRight90Degrees(turned3)

  println(matrix)
  println(turned1)
  println(turned2)
  println(turned3)
  println(turned4)
}
