package challenge

/**
  * [(0,1), (2,1), (0,2), (4,1)] => 1
  *
  *
  */
object TriangleGraph extends App {

  def findTriangleInGraph(input: List[(Int, Int)]): Int = {

    val graphMap: scala.collection.mutable.Map[Int, Set[Int]] = scala.collection.mutable.Map()

    input.foreach(edge => {
       graphMap.get(edge._1) match {
         case None => graphMap.put(edge._1, Set(edge._2))
         case Some(existing) => graphMap.put(edge._1, (existing + edge._2))
       }
    })

    val verticesWithManyEdges = graphMap.filter(_._2.size > 1)
    val edgesWithPotentialTriangle = verticesWithManyEdges.values.toList
    val accumVertices: scala.collection.mutable.Set[(Int, Int)] = scala.collection.mutable.Set()
    val triangularEdges = input.flatMap(edge => {
      edgesWithPotentialTriangle.filter(set => (set.contains(edge._1) && (set.contains(edge._2))))
    })
    triangularEdges.size
  }

  val input1 = List((0,1), (2,1), (0,2), (4,1))
  println(findTriangleInGraph(input1) == 1)

  val input2 = List((0,1), (2,1), (0,2), (4,1), (4, 5))
  println(findTriangleInGraph(input2) == 1)

  val input3 = List((0,1), (2,1), (0,2), (4,1), (4, 2))
  println(findTriangleInGraph(input3) == 2)

  val input4 = List((0,1), (2,1), (0,2), (3,2), (4,2), (4, 3), (5,0))
  println(findTriangleInGraph(input4) == 2)


}
