/**
  * Created by lhendra15 on 1/27/17.
  */
object Fibonacci extends App {

  def f(n: Int) = {

    def foo(accum: List[Int], n: Int): List[Int] = {
      require(accum.size >= 2)

      if (n > 0) {
        val lastTwo = accum.takeRight(2)
        val newElem: Int = lastTwo.head + lastTwo.last
        val newList = accum ::: List(newElem)
        foo(newList, n-1)
      } else {
        accum
      }

    }

    require(n >= 0)

    n match {
      case 0 => List(0)
      case 1 => List(0, 1)
      case _ => foo(List(0, 1), n-1) // n-1 because we start with f(0)
    }


  }

  println(f(0) == List(0))
  println(f(1) == List(0, 1))
  println(f(10) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
  println(f(20) == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765))

}
