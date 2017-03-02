package challenge

/**
  * 1 2 3
  * 4 5 6
  * 7 8 9
  *
  * Diagonal values:
  * 1
  * 2 4
  * 3 5 7
  * 6 8
  * 9
  */
object MatrixDiagonal extends App {

  def printDiagonalValues(matrix: List[List[Int]]): List[List[Int]] = {

    def foo(accum: List[List[Int]], rowsToProcess: List[List[Int]], rowsUnprocessed: List[List[Int]]): List[List[Int]] = {
      rowsToProcess match {
        case Nil => accum
        case _ =>
          val currentDiagonalValues = rowsToProcess.map(_.head)
          val remainderOfCurrentRows = rowsToProcess.map(_.tail).filter(_.nonEmpty)
          val (newRowsToProcess, newRowsUnprocessed) = rowsUnprocessed match {
            case Nil => (remainderOfCurrentRows, Nil)
            case _ => (remainderOfCurrentRows ::: List(rowsUnprocessed.head), rowsUnprocessed.tail)
          }
          foo(accum ::: List(currentDiagonalValues), newRowsToProcess, newRowsUnprocessed)
      }
    }

    foo(List(), List(matrix.head), matrix.tail)
  }

  val input = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val output = List(List(1), List(2, 4), List(3, 5, 7), List(6, 8), List(9))
  println(printDiagonalValues(input) == output)

}
