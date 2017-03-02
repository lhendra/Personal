object FizzBuzz extends App {

  def printIt(i: Int): Unit = {
    val moduloThreeAndOrFive = printModuloThree(i) + printModuloFive(i)
    if (moduloThreeAndOrFive.isEmpty) {
      println(i)
    } else {
      println(moduloThreeAndOrFive)
    }
  }

  def printModuloThree(i: Int): String = {
    if ((i%3 == 0)) {
      "Fizz"
    } else {
      ""
    }
  }

  def printModuloFive(i: Int): String = {
    if ((i%5 == 0)) {
      "Buzz"
    } else {
      ""
    }
  }

  if (!args.isEmpty) {
    val n: Int = args(0).toInt
    1 to n foreach { printIt(_) }
  }

}

