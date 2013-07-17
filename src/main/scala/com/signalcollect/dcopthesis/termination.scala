/*
 *  @author Robin Hafen
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
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package com.signalcollect.dcopthesis


import com.signalcollect.interfaces.AggregationOperation
import com.signalcollect.GlobalTerminationCondition
import com.signalcollect.Vertex
import au.com.bytecode.opencsv.CSVWriter


/*
 * The termination condition used in the evaluation of the algorithms.
 */
class ThesisTerminationCondition(checkInterval: Int)
  extends GlobalTerminationCondition(
      aggregationOperation = new ThesisBenchmarkStatsAggregation,
      aggregationInterval = checkInterval) {

  override def shouldTerminate(stats: ThesisBenchmarkStats): Boolean = {

    // Global logging
    GlobalStats.utilityRatios.append(stats.utilityWithLastState/stats.bestPossibleUtility)

    val nLogs = GlobalStats.utilityRatios.size
    val timeUntilNow = nLogs * checkInterval

    stats match {
      case ThesisBenchmarkStats(util,bestUtil,_,_) if util == bestUtil => {
        
        if (GlobalStats.timeToOptimum == -1) {
          // Shouldn't be necessary since algorithm gets terminated anyway.
          GlobalStats.timeToOptimum = timeUntilNow
        }
        if (GlobalStats.timeToNashEquilibrium == -1) {
          // Value has never been set before, so this is the first time a Nash eq. has been encountered.
          GlobalStats.timeToNashEquilibrium = timeUntilNow
        }

        true // terminate if global optimum found, no point in searching further
      }
      case ThesisBenchmarkStats(util,bestUtil,true,_) => {
        if (GlobalStats.timeToNashEquilibrium == -1) {
          // value has never been set before, so
          // this is the first time a Nash eq has been encountered
          GlobalStats.timeToNashEquilibrium = timeUntilNow
        }

        false // don't terminate on Nash eq. search for better solutions
      }
      case other => false
    }
  }
}


object ThesisBenchmarkStats {
  val zero = ThesisBenchmarkStats(0, 0, true, 0)
  def plus(fst: ThesisBenchmarkStats, snd: ThesisBenchmarkStats) =
    ThesisBenchmarkStats(
      fst.utilityWithLastState + snd.utilityWithLastState,
      fst.bestPossibleUtility + snd.bestPossibleUtility,
      fst.nashEquilibriumReached && snd.nashEquilibriumReached,
      fst.numberVertices + snd.numberVertices)
}
 

/**
 * Holder object for values that get aggregated in certain intervals.
 * Values to be computed from this (the same or similar metrics as were used in
 * "Benchmarking Hybrid Algorithms for Distributed Constraint Optimisation Games - A. C. Chapman et. al.":
 * 1) Average utility at each time point/step.
 * 2) Ratio of satisfied constraints to the actual number of constraints in the graph (== # edges in the case of graph coloring).
 * @param utility The total utility over all vertices in a graph.
 * @param bestPossibleUtility The utility which could theoretically be achieved.
 * @param nashEquilibriumReached If there is no better state (higher utility) for each checked vertex.
 * @param numberVertices The number of vertices for which the above values were computed.
 */
case class ThesisBenchmarkStats(
    utilityWithLastState: Double,
    bestPossibleUtility: Double,
    nashEquilibriumReached: Boolean,
    numberVertices: Int) {
  override def toString = {
      s"""
      | totalUtility: $utilityWithLastState
      | maxUtil: $bestPossibleUtility
      | ratio: ${utilityWithLastState.toDouble/bestPossibleUtility}"
      """.stripMargin
  }
}


/**
 * An aggregation operation which logs the metrics of the thesis.
 */
class ThesisBenchmarkStatsAggregation extends AggregationOperation[ThesisBenchmarkStats] {
  def reduce(elements: Stream[ThesisBenchmarkStats]): ThesisBenchmarkStats = {
    elements.foldLeft(ThesisBenchmarkStats.zero)(ThesisBenchmarkStats.plus)
  }

  def extract(v: Vertex[_, _]): ThesisBenchmarkStats = v match {
      case vertex: ColorConstrainedVertex[_,_] => {
        val utilWithLastState = vertex.lastSignalState map { vertex.utility(_) } getOrElse 0.0
        
        ThesisBenchmarkStats(
            utilWithLastState,
            vertex.bestPossibleUtility,
            !vertex.existsBetterStateUtility,
            1)
      }
      case other => throw new Exception("No ColorConstrainedVertex found " +
        "while logging Thesis Stats!")
  }
}


// The following modules are not used anymore but may prove helpful in future evaluations.

class GlobalUtility extends AggregationOperation[Double] {
  val neutralElement = 0.0

  def extract(v: Vertex[_, _]): Double = v match {
    case vertex: ColorConstrainedVertex[_, _] => vertex.currentUtility
    case other => throw new Exception("No ColorConstrainedVertex found during" +
      " extraction in GlobalUtility-aggregationOperation!")
  }

  def reduce(elements: Stream[Double]): Double = elements.sum
}


class NumberNonSatisfied extends AggregationOperation[Int] {
  val neutralElement = 0
  def extract(v: Vertex[_, _]): Int = v match {
    case vertex: ColorConstrainedVertex[_, _] =>
      vertex.constraintsCount - vertex.currentNumberSatisfied
    case other => throw new Exception("No ColorConstrainedVertex found during" +
      "extraction in NumberNonSatisfied-aggregationOperation!")
  }
  def reduce(elements: Stream[Int]) = elements.sum
}


class GlobalUtilityConvergedCondition(checkInterval: Long, thresholdUtility: Double)
  extends GlobalTerminationCondition(
    aggregationOperation = new GlobalUtility,
    aggregationInterval = checkInterval) {

  override def shouldTerminate(globalUtility: Double) = {
      if (globalUtility >= thresholdUtility) true else false
  }
}


class NashEquilibrium extends AggregationOperation[Boolean] {
  val neutralElement = true
  def extract(v: Vertex[_, _]): Boolean = v match {
    case vertex: ColorConstrainedVertex[_,_] => !vertex.existsBetterStateUtility
    case other => throw new Exception("No ColorConstrainedVertex found during" +
      "aggregation while checking for NashEquilibrium!")
  }
  def reduce(elements: Stream[Boolean]) = elements.foldLeft(neutralElement)(_ && _)
}


class AllSatisfiedCondition(checkInterval: Long)
  extends GlobalTerminationCondition(aggregationOperation = new NumberNonSatisfied) {

  override def shouldTerminate(numberNonSatisfied: Int) = {
    if (numberNonSatisfied == 0) true else false
  }

}
