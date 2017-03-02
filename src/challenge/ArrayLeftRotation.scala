package challenge

/**
  * Created by lhendra15 on 2/3/17.
  */
object ArrayLeftRotation {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in)
    var n = sc.nextInt()
    var k = sc.nextInt()
    var a = new Array[Int](n)
    for(a_i <- 0 to n-1) {
      a(a_i) = sc.nextInt()
    }

    val (left, right) = a.splitAt(k-1)
    val resultArray = right ++ left
    resultArray.foreach(print(_))
  }

}
