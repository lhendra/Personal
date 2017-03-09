package challenge

object StringReplacement extends App {

  def replaceAll(input: String, replacements: Map[String, String]): String = {

    def replace(str: String, accum: String, replacement: (String, String)): String = {

      if (str.isEmpty) {
        accum
      } else {
        val foundIndex = str.indexOf(replacement._1)
        if (foundIndex >= 0) {
          val left = str.substring(0, foundIndex)
          val remaining = str.substring(foundIndex + replacement._1.size)
          val newAccum = accum + (left + replacement._2)
          replace (remaining, newAccum, replacement)
        } else {
          accum + str
        }
      }

    }

    var newStr = input
    replacements.foreach(replacement => {
      newStr = replace(newStr, "", replacement)
    })

    newStr

  }

  val input = "Hello, NAME. You have COUNT messages, NAME."
  val replacements = Map("NAME" -> "Jimmy", "COUNT" -> "10", "Hello" -> "Good morning")

  println(replaceAll(input, replacements))

}
