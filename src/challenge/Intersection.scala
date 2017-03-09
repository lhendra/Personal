package challenge

/**
  * Given 2 different lists of integers, write a function that will return their intersection.
  */
object Intersection extends App {

  var intMap: Map[Int, Int] = Map[Int, Int]()

  def intersect(set1: Set[Int], set2: Set[Int]): List[Int] = {

    def addAllElements(set: Set[Int]) = {
      set.foreach(value => {
        intMap.get(value) match {
          case None =>
            intMap += (value -> 1)
          case Some(existingCount) =>
            intMap += (value -> (existingCount+1))
        }
      })
    }

    addAllElements(set1)
    addAllElements(set2)
    intMap.iterator.toList.filter(_._2 > 1).map(_._1)
  }

  val list1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 3, 4, 5)
  val list2 = List(3, 9, 10, 8, 10, 15, 20)
  println(intersect(list1.toSet, list2.toSet))
}
