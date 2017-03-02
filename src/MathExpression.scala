import scala.collection.mutable.{ListBuffer, Stack}

trait Operator {
  def calculate(o1: Int, o2: Int): Int
}
case class Plus() extends Operator {
  def calculate(o1: Int, o2: Int) = o1 + o2
}
case class Minus() extends Operator {
  def calculate(o1: Int, o2: Int) = o1 - o2
}
case class Multiply() extends Operator {
  def calculate(o1: Int, o2: Int) = o1 * o2
}
case class Divide() extends Operator {
  def calculate(o1: Int, o2: Int) = o1 / o2
}


case class MathExpression() {

  val operandStack = Stack[Int]()
  val operatorStack = Stack[Operator]()

  var tempOperand: ListBuffer[Char] = ListBuffer.empty

  def calculate(input: Array[Char]): Int = {

    def persistOperand() = {
      if (tempOperand != ListBuffer.empty) {
        operandStack.push(tempOperand.mkString.toInt)
        tempOperand = ListBuffer.empty
      }
    }

    input.foreach(elem => {
      elem match {
        case '(' => persistOperand
        case ')' =>
          persistOperand
          val o2 = operandStack.pop
          val o1 = operandStack.pop
          val newOperand = operatorStack.pop.calculate(o1, o2)
          operandStack.push(newOperand)
        case '+' =>
          persistOperand
          operatorStack.push(Plus())
        case '-' =>
          persistOperand
          operatorStack.push(Minus())
        case '*' =>
          persistOperand
          operatorStack.push(Multiply())
        case '/' =>
          persistOperand
          operatorStack.push(Divide())
        case x =>
          tempOperand += x
      }
    })
    operandStack.pop
  }

}

object MathExpression extends App {

  println(MathExpression().calculate("(16+((((1+2)*3)/4)-5))".toCharArray))

}