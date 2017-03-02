import scala.collection.mutable

/**
  * Created by lhendra15 on 1/30/17.
  */
case class Tower[T](index: Int) {

  val disks = mutable.Stack[T]()

  def moveDisks(n: Int, destination: Tower[T], buffer: Tower[T]): Unit = {
    if (n > 0) {
      moveDisks(n-1, buffer, destination)
      moveTop(destination)
      buffer.moveDisks(n-1, destination, this)
    }
  }

  def moveTop(destination: Tower[T]) = {
    if (!disks.isEmpty) {
      println(s"Moving ${disks.top} from Stack $index to Stack ${destination.index}")
      destination.add(disks.pop())
    }
  }

  def add(disk: T) = {
    disks.push(disk)
    println(s"Tower $index: $disks")
  }

}

object TowerOfHanoi extends App {

  val tower1 = Tower[Int](1)
  val tower2 = Tower[Int](2)
  val tower3 = Tower[Int](3)

  5 to 1 by -1 foreach {
    tower1.add(_)
  }

  tower1.moveDisks(5, tower3, tower2)

}