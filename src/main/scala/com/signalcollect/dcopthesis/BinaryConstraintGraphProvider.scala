/*
 *  @author Robin Hafen
 *
 *  Copyright 2013 University of Zurich
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.signalcollect.dcopthesis

import scala.collection.mutable.LinkedHashSet
import scala.xml._
import com.signalcollect.GraphEditor
import com.signalcollect.Vertex
import com.signalcollect.Edge
import com.signalcollect.dcopthesis.Util._
import java.io.File
import java.io.PrintWriter
import java.io.FileNotFoundException


object FileDialect extends Enumeration {
  type FileDialect = Value
  val XMLFile, EdgeListFile = Value
}

import FileDialect._

/** This GraphProvider constructs a graph with a given tightness.
  * @constructor Create a GraphProvider which constructs a graph via its populate method.
  * @param graphSize Number of nodes the graph should have.
  * @param meanDegree The mean degree the graph should have.
  * @param domainSize Number of different states that a vertex can take. A domain size of 10 would
  * result in a domain (0,1,2,3,4,5,6,7,8,9).
  */
class BinaryConstraintGraphProvider(
    val graphSize: Int,
    val meanDegree: Double,
    val domainSize: Int,
    val saveTo: Option[String] = None,
    val loadFrom: Option[(String, FileDialect)] = None)
  extends ConstraintGraphProvider[Any, Any] {

  type Degree = Double

  def numberToPrunePerStep(currentAvgDegree: Double) = {
    if (currentAvgDegree < meanDegree + 20) {
      1
    } else {
      graphSize
    }
  }

  def fromEdgeList(filename: String): (LinkedHashSet[Set[Int]], Degree) = {
    val undirectedEdges = LinkedHashSet[Set[Int]]()
    try {
      val src = io.Source.fromFile(filename)
      src.getLines foreach { line =>
        val Array(iStr, jStr) = line.split(" ")
        if (iStr != jStr) {
          undirectedEdges.add(Set(iStr.toInt, jStr.toInt))
        }
      }
    } catch {
      case e: FileNotFoundException => throw e
    }
    (undirectedEdges, -1.0)
  }

  def fromXML(filename: String): (LinkedHashSet[Set[Int]], Degree) = {
    val undirectedEdges = LinkedHashSet[Set[Int]]()

    println(s"[info] loading graph from file: $filename")

    var actualDegree = 0.0
    try {
      val src = io.Source.fromFile(filename)
      val graph = XML loadString src.mkString

      val doubleEdges = graph \\ "double-edge"
      actualDegree = (graph \ "@actual-degree" text).toDouble

      val undirectedEdges = doubleEdges map { edge =>
        val vertexIds = (edge \ "vid").theSeq

        if (vertexIds.size != 2)
          throw new Exception("Invalid number of vid in double-edge node")

        val i = vertexIds(0).text.toInt
        val j = vertexIds(1).text.toInt
        Set(i,j)
      }

      val asHashSet = LinkedHashSet[Set[Int]](undirectedEdges:_*)

      (asHashSet, actualDegree)

    } catch {
      case e: FileNotFoundException => throw e
    }
  }

  def toFile(undirectedEdges: LinkedHashSet[Set[Int]], actualDegree: Double, filename: String) {
    val graph =
      <graph size={ graphSize.toString } target-degree={ meanDegree.toString } actual-degree={ actualDegree.toString }>
      {
        for (edge <- undirectedEdges) yield {
          val List(i, j) = edge.toList
          <double-edge><vid>{ i }</vid><vid>{ j }</vid></double-edge>
        }
      }
      </graph>

      val pp = new PrettyPrinter(80,2)
      val sb = new StringBuilder()
      pp.format(graph, sb)
      val pw = new PrintWriter(new File(filename))
      pw.write(sb.mkString)
      pw.close
  }

  def computeEdgeList(): (LinkedHashSet[Set[Int]], Degree) = {
    import scala.util.Random

    var vertexList: List[Vertex[Any,_]] = List()

    val rand = Random

    val is = 0 until graphSize toList

    // A fully connected graph represented by a list of sets containing two vertex ids {i, j}
    val conn: List[Set[Int]] =
      List.fill(2)(is).flatten.combinations(2)
                      .map { xs => Set(xs(0), xs(1)) }
                      .filterNot { _.size == 1 }
                      .toList

    // Convert it to an ordered mutable Hash set
    val undirectedEdges = LinkedHashSet[Set[Int]]( conn:_* )

    def computeAvgDegree(is: Iterable[Int], edges: LinkedHashSet[Set[Int]]): Double = {
      val degs = is map { i =>
        edges count { edge => edge contains i }
      }
      degs.sum / is.size.toDouble
    }

    def removeRandomEdge(edges: LinkedHashSet[Set[Int]]) {
      val elem = rand.shuffle(undirectedEdges).head
      val List(i, j) = elem.toList
      val otherEdges = undirectedEdges filter { _ != elem }
      val notLastEdgeI = otherEdges exists { edge => edge(i) }
      val notLastEdgeJ = otherEdges exists { edge => edge(j) }
      if (notLastEdgeI && notLastEdgeJ) {
        edges.remove(elem)
      } else {
        throw new Exception("This is the last edge for one of the involved vertices!")
      }
    }

    var keepRemoving = true
    var currentAvgDegree = -1.0
    while (keepRemoving) {
      currentAvgDegree = computeAvgDegree(is, undirectedEdges)
      try {
        numberToPrunePerStep(currentAvgDegree) times {
          removeRandomEdge(undirectedEdges)
        }
      } catch {
        case e: Throwable => ()//println("[warning] trying to remove last edge for a vertex")
      }
      if (currentAvgDegree < meanDegree) {
        keepRemoving = false
        println("[info] Graph built with final average degree: " + currentAvgDegree)
      }
    }

    (undirectedEdges, currentAvgDegree)
  }

  def populate(graphEditor: GraphEditor[Any, Any],
               vertexBuilder: VertexBuilder,
               constraintEdgeBuilder: (Int, Int) => Edge[Int]): Unit = {

    // Load a previously computed and saved graph from file
    // or calculate a new one
    val (undirectedEdges, actualDegree) = {
      loadFrom map {
        case (fname, XMLFile) => fromXML(fname)
        case (fname, EdgeListFile) => fromEdgeList(fname)
      } getOrElse computeEdgeList()
    }

    // Save to file if a filename was supplied
    saveTo foreach { fname =>
      toFile(undirectedEdges, actualDegree, fname)
    }

    // Add vertices to the graph
    for (i <- 0 until graphSize) {
      val id: Int = i
      val domain = (0 until domainSize).toArray
      val vertex = vertexBuilder(id, domain)
      graphEditor.addVertex(vertex)
    }

    // Add edges accoring to constraint map and set the each vertex' constraints
    for (edge <- undirectedEdges) {
      val edgeList = edge.toList
      val i = edgeList(0)
      val j = edgeList(1)
      graphEditor.addEdge(i, constraintEdgeBuilder(i, j))
      graphEditor.addEdge(j, constraintEdgeBuilder(j, i))
    }
  }
}
