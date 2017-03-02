package challenge

//* ['c', 'f', 'j', 'p', 'v'], 'a' => 'c'
//
//'j' -> > 'a'
//[c, f, j] => f > 'a'
//[c, f] => c > a
//[c] => return c
//
//
//* ['c', 'f', 'j', 'p', 'v'], 'c' => 'f'
//j > c
//[c, f, j] =>
//[c, f, j] => f > 'c'
//[c, f] => c = c
//[f] => return f
//
//
//* ['c', 'f', 'j', 'p', 'v'], 'k' => 'p'
//j < p
//[j, p, v] => p > k
//[j, p] => j > k
//[p] => p > k && p is the only in the list, return p
//
//
//* ['c', 'f', 'j', 'p', 'v'], 'z' => 'c' // The wrap around case
//:
//:
//[v] v < z
//originalList.head
//
//* ['c', 'f', 'k'], 'f' => 'k'
//* ['c', 'f', 'k'], 'c' => 'f'
//* ['c', 'f', 'k'], 'd' => 'f'
//*/
//
//0 1 2 3 4 5 => 6
//6/2


object InsertionPoint extends App {

  def findNextGreater(sortedString: String, x: Char): Char = {

    def foo(charList: List[Char]): Char = {
      charList match {
        case Nil => sortedString.head
        case head::Nil =>
          if (head <= x) {
            sortedString.head // we're wrapping around if x > than all elements in list
          } else {
            head
          }
        case _ =>
          val (left, right) = charList.splitAt(charList.size/2)
          if (left.last <= x && right.head > x) {
            right.head
          } else if (left.last <= x && right.head <= x) {
            foo(right)
          } else {
            foo(left)
          }
      }
    }

    val temp = foo(sortedString.toCharArray.toList)
    println(s"insertion point: $temp")
    temp

  }

  println(findNextGreater("cfjpv", 'a') == 'c')
  println(findNextGreater("cfjpv", 'c') == 'f')
  println(findNextGreater("cfjpv", 'k') == 'p')
  println(findNextGreater("cfjpv", 'z') == 'c')

}

