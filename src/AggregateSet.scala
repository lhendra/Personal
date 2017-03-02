case class AggregateSet(initialInput: Set[Int]) {

  private def getSetOfBigger(input: Set[Int]): Set[Int] = {
    initialInput.filter(_ > (input + Int.MinValue).max)
  }

  private def aggregate(input: Set[Set[Int]]): Set[Set[Int]] = {

    if (input.exists(_ == initialInput)) {
      input
    } else {
      val thisIteration = input.flatMap(elem => {
        val toAdd = getSetOfBigger(elem)
        toAdd.map(elem + _)
      })
      thisIteration ++ aggregate(thisIteration)
    }

  }

  def calculate() = {
    aggregate(Set(Set.empty))
  }
}


object AggregateSet {

  def main(args: Array[String]): Unit = {
    println(AggregateSet(Set(1, 2, 3, 4, 5)).calculate)
  }

}