package challenge

/**
  * Created by lhendra15 on 2/4/17.
  *
  * 1 2 3
  * 4 5 6
  * 7 8 9
  *   |
  *   |
  * 1 4 7
  * 2 5 8
  * 3 6 9
  */
object MatrixTranspose extends App {

  val matrix: List[List[Int]] = List(List(1,2,3), List(4, 5, 6), List(7, 8, 9))

  def transpose(accumResult: List[List[Int]], listOfList: List[List[Int]]): List[List[Int]] = {
    if (listOfList.head.isEmpty) {
      accumResult
    } else {
      val thisRow = listOfList.map(_.head)
      val nextRows = listOfList.map(_.tail)
      transpose(accumResult ::: List(thisRow), nextRows)
    }
  }

  val transposed1 = transpose(List(), matrix)
  val transposed2 = transpose(List(), transposed1)
  val transposed3 = transpose(List(), transposed2)
  val transposed4 = transpose(List(), transposed3)
  println(transposed1)
  println(transposed2)
  println(transposed3)
  println(transposed4)

}
