package datastructure

case class Entry[K, V](key: K, value: Option[V])

case class HashMap[K, V]()(implicit ordering:Ordering[Entry[K, V]]) {

  private val bucketSize = 256
  private val storage = new Array[BinarySearchTree[Entry[K, V]]](bucketSize)

  def put(key: K, value: V) = {
    val index = key.hashCode % bucketSize
    var currentBucket = storage(index)
    if (currentBucket == null) {
      currentBucket = new BinarySearchTree[Entry[K, V]]()
    }
    currentBucket.insert(Entry(key, Some(value)))
    storage.update(index, currentBucket)
  }

  def get(key: K): Option[V] = {
    val index = key.hashCode % bucketSize
    storage(index).find(Entry(key, None)).flatMap(_.value.value)
  }

  def delete(key: K): Unit = {
    val index = key.hashCode % bucketSize
    val currentBucket = storage(index)
    currentBucket.delete(Entry(key, None))
  }

  def getAll(): List[Entry[K, V]] = {
    storage.foldLeft(List[Entry[K, V]]())((accum, elem) => {
      val current = if (elem != null) {
        elem.traverseInOrder().getOrElse(List())
      } else {
        List()
      }
      accum ::: current
    })
  }
}

object HashMap extends App {

  val entryOrdering = new Ordering[Entry[Int, String]] {
    def compare(a:Entry[Int, String],b:Entry[Int, String]) = b.key.hashCode - a.key.hashCode
  }

  val map = new HashMap[Int, String]()(entryOrdering)
  map.put(1, "one")
  map.put(10, "ten")
  map.put(5, "five")
  map.put(3, "three")
  map.put(2, "two")
  map.put(8, "eight")
  map.put(9, "nine")
  map.put(9, "nine")
  map.put(10, "ten")

  println(map.getAll)

}