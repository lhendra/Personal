package challenge

//Alice is taking a cryptography class and finding anagrams to be very useful. We consider two strings to be anagrams of each other if the
//first string's letters can be rearranged to form the second string. In other words, both strings must contain the same exact letters in the
// same exact frequency For example, bacdc and dcbac are anagrams, but bacdc and dcbad are not.
//
//Alice decides on an encryption scheme involving two large strings where encryption is dependent on the minimum number of character deletions
// required to make the two strings anagrams. Can you help her find this number?
//
//Given two strings,  and , that may or may not be of the same length, determine the minimum number of character deletions required to make  and
//  anagrams. Any characters can be deleted from either of the strings.
//
//1: cde
//2: abc
//anagram chars: c
//delete 4 chars: d, e from 1; a, b from 2
//
//1: abcde
//2: bcd
//anagram chars: bcd
//delete 2 chars: a, e from 1, none from 2
//
//1: abccde
//2: cf
//anagram chars: c
//delete 6 chars: abcde from 1, f from 2
//
//HashMap[Char, Int]
//go through list 1, add to map for every character, keeping counter of how many each char appears
//go through list 2, if a char exists in the map, decrease the count. Otherwise add that char to the map

case class AnagramsOfEachOther() {

  val charMap: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map[Char, Int]()

  def charactersToRemove(list1: List[Char], list2: List[Char]): List[Char] = {
    list1.foreach(char => {
      charMap.get(char) match {
        case None => charMap.put(char, 1)
        case Some(count) => charMap.put(char, count + 1)
      }
    })

    list2.foreach(char => {
      charMap.get(char) match {
        case None => charMap.put(char, 1)
        case Some(count) =>
          if (count > 1) {
            charMap.put(char, count - 1)
          } else {
            charMap.remove(char)
          }
      }
    })


    charMap.iterator.flatMap(elem => {
      List.fill(elem._2)(elem._1)
    }).toList
  }

  def numCharactersToRemove(list1: List[Char], list2: List[Char]): Int = {
    charactersToRemove(list1, list2).size
  }

}

object AnagramsOfEachOther extends App {

  println(AnagramsOfEachOther().charactersToRemove(List('c', 'd', 'e'), List('a', 'b', 'c')))
  println(AnagramsOfEachOther().charactersToRemove(List('a', 'b', 'c', 'd', 'e'), List('b', 'c', 'd')))
  println(AnagramsOfEachOther().charactersToRemove(List('a', 'b', 'c', 'c', 'd', 'e'), List('c', 'f')))

}
