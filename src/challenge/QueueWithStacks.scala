package challenge

/**
  * Implement a queue using 2 stacks. Specifically, implement the enqueue and dequeue methods.
  */
case class QueueWithStacks[T]()(implicit ordering: Ordering[T]) {

  val stack1 = scala.collection.mutable.Stack[T]()
  val stack2 = scala.collection.mutable.Stack[T]()


  def enqueue(item: T) = {
    if (stack1.isEmpty) {
      while (stack2.nonEmpty) {
        val popped = stack2.pop
        stack1.push(popped)
      }
      stack1.push(item)
    } else {
      stack1.push(item)
    }
  }

  def dequeue(): Option[T] = {

    def pop() = {
      if (stack2.nonEmpty) {
        Some(stack2.pop)
      } else {
        None
      }
    }

    if (stack2.isEmpty) {
      while (stack1.nonEmpty) {
        val popped = stack1.pop
        stack2.push(popped)
      }
      pop
    } else {
      pop
    }
  }

}

object QueueWithStacks extends App {

  val queue = QueueWithStacks[Int]()
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  queue.enqueue(10)
  queue.enqueue(9)
  queue.enqueue(8)

  println(queue.dequeue())
  println(queue.dequeue())
  println(queue.dequeue())
  println(queue.dequeue())
  println(queue.dequeue())
  println(queue.dequeue())
  println(queue.dequeue())
}
