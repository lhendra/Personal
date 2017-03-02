package challenge

/**
  * [(1, 4)] <- (11, 15)
  * [(1, 4), (11, 15)]
  *
  * [(1, 4), (11, 15)] <- (6, 8)
  * [(1, 4), (6, 8), (11, 15)]
  *
  * [(1, 4), (6, 8), (11, 15)] <- (16, 20)
  * result: [(1, 4), (6, 8), (11, 15), (16, 20)]
  *
  * [(1, 4), (6, 8), (11, 15)] <- (7, 8)
  * result: [(1, 4), (6, 8), (11, 15)]
  *
  * [(1, 4), (6, 8), (11, 15)] <- (7, 9)
  * result: [(1, 4), (6, 9), (11, 15)]
  *
  * [(1, 4), (6, 8), (11, 15)] <- (8, 11)
  * result: [(1, 4), (6, 15)]
  */

case class Range(x: Int, y: Int) {
  def count() = y-x

  def intersectLeft(left: Range) = {
    (x >= left.x) && (x <= left.y)
  }

  def intersectRight(right: Range) = {
    (y >= right.x) && (y <= right.y)
  }
}

case class Interval() {

  var state: List[Range] = List()

  def findInsertionPoint(newRange: Range): (List[Range], List[Range]) = {
    val left = state.filterNot(element => (element.x >= newRange.y))
    val right = state.filterNot(element => (element.y <= newRange.y))
    (left, right)
  }

  def insertAndUpdateState(left: List[Range], right: List[Range], newRange: Range): Unit = {
    state = (left.lastOption, right.headOption) match {
      case (None, None) =>
        // nothing in the state yet, so just add the new range
        left ::: List(newRange) ::: right
      case (None, Some(r)) =>
        // should be inserted in the front of the list
        if (newRange.intersectRight(r)) {
          List(Range(Math.min(r.x, newRange.x), Math.max(r.y, newRange.y))) ::: right.tail
        } else {
          newRange :: right
        }
      case (Some(l), None) =>
        // should be inserted in the back of the list
        if (newRange.intersectLeft(l)) {
          left.dropRight(1) ::: List(Range(Math.min(l.x, newRange.x), Math.max(l.y, newRange.y)))
        } else {
          left ::: List(newRange)
        }
      case (Some(l), Some(r)) =>
        // should be inserted in the middle of the list
        (newRange.intersectLeft(l), newRange.intersectRight(r)) match {
          case (true, true) =>
            val middle = List(Range(Math.min(l.x, newRange.x), Math.max(r.y, newRange.y)))
            left.dropRight(1) ::: middle ::: right.tail
          case (false, true) =>
            val middle = List(Range(Math.min(r.x, newRange.x), Math.max(r.y, newRange.y)))
            left ::: middle ::: right.tail
          case (true, false) =>
            val middle = List(Range(Math.min(l.x, newRange.x), Math.max(l.y, newRange.y)))
            left.dropRight(1) ::: middle ::: right
          case (false, false) =>
            left ::: List(newRange) ::: right
        }
    }
  }

  def getTotal(newRange: Range) = {
    val (left, right) = findInsertionPoint(newRange)
    insertAndUpdateState(left, right, newRange)
    val total = state.foldLeft(0)((accum, currentRange) => accum + currentRange.count)
    println(s"Adding: $newRange... Intervals so far: $state. Total: $total")
  }

}

object Interval extends Interval with App {

  getTotal(new Range(1, 4))
  getTotal(new Range(11, 15))
  getTotal(new Range(6, 8))
  getTotal(new Range(16, 20))
  getTotal(new Range(7, 8))
  getTotal(new Range(7, 9))
  getTotal(new Range(8, 11))

}
