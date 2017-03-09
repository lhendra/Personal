package datastructure

import scala.collection.mutable.HashSet

case class Edge(fromId: String, toId: String)
case class Vertex[T](id: String, value: T)

case class VertexInfo(id: String, isRoot: Boolean = true)

class GraphDirected[T]() {

  private val vertexList = scala.collection.mutable.Set[Vertex[T]]()
  private val adjMatrix = new scala.collection.mutable.HashMap[VertexInfo, HashSet[String]]()
  private val toListOuterJoinFromList = scala.collection.mutable.Set[String]()

  def addVertex(newVertex: Vertex[T]) = {
    require (!vertexList.exists(_.id == newVertex.id))
    vertexList += newVertex
  }

  def removeVertex(vertexId: String) = {
    vertexList.find(_.id == vertexId) match {
      case None => // do nothing
      case Some(foundVertex) => vertexList -= foundVertex
    }
  }

  def addEdge(newEdge: Edge) = {

    val fromEdgeNotRoot: VertexInfo = VertexInfo(newEdge.fromId, isRoot = false)
    val fromEdge: VertexInfo = if (toListOuterJoinFromList.contains(newEdge.fromId)) {
      toListOuterJoinFromList -= newEdge.fromId
      fromEdgeNotRoot
    } else {
      if (adjMatrix.contains(fromEdgeNotRoot)) {
        fromEdgeNotRoot
      } else {
        VertexInfo(newEdge.fromId)
      }
    }

    val newAdjEdges: HashSet[String] = adjMatrix.get(fromEdge).getOrElse(HashSet[String]()) + (newEdge.toId)
    adjMatrix.put(fromEdge, newAdjEdges)

    adjMatrix.remove(VertexInfo(newEdge.toId)) orElse(adjMatrix.remove(VertexInfo(newEdge.toId, isRoot = false))) match {
      case None =>
        toListOuterJoinFromList += newEdge.toId
      case Some(existingAdjList) =>
        adjMatrix.put(VertexInfo(newEdge.toId, isRoot = false), existingAdjList)
    }
  }

  def printAdjList() = {
    adjMatrix.iterator.foreach(entry => {
      println(s"${entry._1}: [${entry._2.toList}]")
    })
  }

  def depthFirst() = {

    val visited = new HashSet[String]()

    def df(from: String): Unit = {
      if (!visited.contains(from)) {
        val adjVertices = adjMatrix.getOrElse(VertexInfo(from), adjMatrix.getOrElse(VertexInfo(from, isRoot = false), HashSet[String]()))
        adjVertices.foreach(to => {
          println(s"$from -> $to")
          df(to)
        })
        visited.add(from)
      }
    }

    adjMatrix.keysIterator.filter(_.isRoot).foreach(element => df(element.id))
  }

  def findDepthFirstStack(vertexToFind: String) = {

    val visited = new HashSet[String]()
    val stack = scala.collection.mutable.Stack[String]()

    def getAllAdj(vertexId: String) = {
      (adjMatrix.get(VertexInfo(vertexId)) orElse (adjMatrix.get(VertexInfo(vertexId, isRoot = false)))).getOrElse(HashSet())
    }

    def getUnvisitedAdj(vertexId: String): HashSet[String] = {
      getAllAdj(vertexId).flatMap(item => {
        if (visited.contains(item)) {
          None
        } else {
          Some(item)
        }
      })
    }

    def ds(vertexId: String): Boolean = {
      stack.push(vertexId)
      val adj = getAllAdj(vertexId)
      if (adj.nonEmpty) {
        val to = adj.head
        println(s"Looking at ${vertexId} -> $to")
        if (to == vertexToFind) {
          true
        } else {
          ds(to)
        }
      } else {
        nextDs
      }
    }

    def nextDs(): Boolean = {
      if (stack.nonEmpty) {
        val processed = stack.pop
        visited.add(processed)
        if (stack.nonEmpty) {
          val adj = getUnvisitedAdj(stack.top)
          if (adj.nonEmpty) {
            ds(adj.head)
          } else {
            false
          }
        } else {
          // we are done - can't find anything!
          false
        }
      } else {
        false
      }
    }

    adjMatrix.keysIterator.filter(_.isRoot).foreach(element => ds(element.id))

  }

  def breadthFirst() = {

    val visited = new HashSet[String]()

    def bf(root: String) = {
      val toVisit = scala.collection.mutable.Queue[String](root)

      while (toVisit.nonEmpty) {
        val from = toVisit.dequeue()
        if (!visited.contains(from)) {
          visited.add(from)
          val adjVertices = adjMatrix.getOrElse(VertexInfo(from), adjMatrix.getOrElse(VertexInfo(from, isRoot = false), HashSet[String]()))
          adjVertices.foreach(to => {
            println(s"$from -> $to")
            toVisit.enqueue(to)
          })
        }
      }
    }

    adjMatrix.keysIterator.filter(_.isRoot).foreach(element => bf(element.id))
  }

}

object GraphDirected extends App {

  val g = new GraphDirected[Int]()
  g.addVertex(Vertex("zero", 0))
  g.addVertex(Vertex("one", 1))
  g.addVertex(Vertex("two", 2))
  g.addVertex(Vertex("three", 3))
  g.addVertex(Vertex("four", 4))
  g.addVertex(Vertex("five", 5))

  g.addEdge(Edge("zero", "two"))
  g.addEdge(Edge("one", "three"))
  g.addEdge(Edge("two", "three"))
  g.addEdge(Edge("two", "four"))
  g.addEdge(Edge("three", "five"))
  g.addEdge(Edge("four", "five"))
  g.printAdjList

  println("Depth First:")
  g.depthFirst

  println("Depth First with Stack: looking for four")
  g.findDepthFirstStack("four")

  println("Breadth First:")
  g.breadthFirst
}
