/*
 *  @author Daniel Strebel
 *  @author Philip Stutz
 *
 *  Copyright 2012 University of Zurich
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */
package com.signalcollect.dcopthesis

import com.signalcollect._
import com.signalcollect.interfaces.AggregationOperation
import com.signalcollect.ExecutionInformation
import com.signalcollect.dcopthesis._
import com.signalcollect.ExecutionInformation
import com.signalcollect.evaluation.EvaluationAlgorithmRun

class DcopEvaluationRun(
  val algorithmName: String,
  graphBuilder: GraphBuilder[Any, Any],
  edgeBuilder: (Int, Int) => Edge[Int],
  vertexBuilder: VertexBuilder,
  graphProvider: ConstraintGraphProvider[Any, Any],
  executionConfiguration: ExecutionConfiguration,
  jvmParams: String = "",
  reportMemoryStats: Boolean = false) extends EvaluationAlgorithmRun[Any, Any] {

  var stats: ExecutionInformation = null

  def loadGraph = {
    graphProvider.populate(graph, vertexBuilder, edgeBuilder)
    graph.awaitIdle
    /*
    println("-----Graph Provider-----")
    println("Printing vertex states")
    graph.foreachVertex(println(_))
    println("Done printing vertex states")
    println("------------------------")
    readLine
    */
  }

  //TODO: Cut the AdoptFileGraphGenerator: creating a graph instance to use in buildGraph, and adding edges and vertices for loadGraph

  def buildGraph = {
    graph = graphBuilder.build
  }

  def execute = {
//    println("Before aggregate")
//    println("Printing vertex states")
//    graph.foreachVertex(println(_))
//    println("Done printing vertex states")
//        graph.foreachVertex({
//          case vertex: ColorConstrainedVertex[Any, _] => {
//            val neighbourIds = vertex.getTargetIdsOfOutgoingEdges
//            println(s"Added ${vertex.numberOfConstraints} constraints to vertex ${vertex.id}")
//            graph
//          }
//          case other => throw new Exception("These are non-Constraint Vertexes")
//        })
//    readLine
    stats = graph.execute(executionConfiguration)
    stats
  }

  override def postExecute(statsDummy: Map[String, String]): List[(String, String)] = {
    GlobalStats.plot.addLinePlot(
      statsDummy("algorithm"),
      (0 until GlobalStats.utilityRatios.size).map(_.toDouble).toArray,
      GlobalStats.utilityRatios.toArray)
    /*
    val pseudoAggregate = graph.aggregate(new GlobalUtility)
    List[(String, String)](
        ("utility", pseudoAggregate.toString),
        ("domainSize", graphProvider.domainSize.toString),
        ("graphSize", graphProvider.graphSize.toString)
        //("debug", if (stats.aggregatedWorkerStatistics.numberOfVertices < 101) graph.aggregate(new Visualizer).toString else " ")
    )
    */
    List()
  }

  def graphStructure = graphProvider.toString

  override def jvmParameters = jvmParams

  override def memoryStatsEnabled = reportMemoryStats

}

class Visualizer extends AggregationOperation[List[(Any, String)]] {
  val neutralElement: List[(Any, String)] = List()
  def extract(v: Vertex[_, _]): List[(Any, String)] = List((v.id, v.state.toString))
  def reduce(elements: Stream[List[(Any, String)]]): List[(Any, String)] = elements.foldLeft(neutralElement)(aggregate) // use foldRight with cons ?
  def aggregate(a: List[(Any, String)], b: List[(Any, String)]): List[(Any, String)] = a ++ b

}
