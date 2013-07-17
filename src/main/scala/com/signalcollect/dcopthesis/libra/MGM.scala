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

/* maximum-gain messaging algorithm */


package com.signalcollect.dcopthesis.libra


import com.signalcollect.Vertex
import scala.util.Random
import com.signalcollect.dcopthesis.libra.components._
import MGMVertex._
import com.signalcollect.dcopthesis.ColorConstrainedVertex
import com.signalcollect.dcopthesis.VertexBuilder
import scala.Int.int2double
import com.signalcollect.dcopthesis.Utility
import com.signalcollect.DefaultEdge


object MGMVertex {
  type Gain = Double
  type State = Int
  type MGMSignal = Either[Gain, State]
}


object MGMPhase extends Enumeration {
  type MGMPhase = Value
  val maxGainExchange, stateChangeInform = Value
}


/**
 * Simple holder class for storing computer max gains for later transmission
 */
case class MaxGain(gain: Gain = 0, state: State = 0) {
  def withState(newState: State) = MaxGain(gain, newState)
  def withGain(newGain: State) = MaxGain(state, newGain)
}

class MGMVertex(
    newId: Int,
    initialState: Int,
    newDomain: Array[Int])
  extends ColorConstrainedVertex[Int,Int](newId, initialState, newDomain)
  with ArgmaxADecision[Int]
  with CompleteSearch[Int]
  with MapUtilityTarget[Int]
  with FloodSchedule {
  import MGMPhase._

  val weights = scala.collection.mutable.HashMap[Any, Double](
    (neighbourIds map ((_,1.0))).toSeq:_*)

  var phase = stateChangeInform

  var computedMaxGain: Option[MaxGain] = None

  var recievedStateUpdates = Map[Any, State]()
  var recievedGainUpdates = Map[Any, Gain]()

  type Signal = MGMSignal

  override def utility: State => Utility = st => {
    (recievedStateUpdates filter {_._2 != st}).size
  }

  override def numberSatisfiedWith(st: State): Int = utility(st).toInt


  override def collect: Int = {

    stepCounter += 1

    val gainSignals = mostRecentSignalMap filter { _._2.isLeft } map {
      case (id, Left(gain)) => (id, gain)
      case otherwise => throw new Error("Left expected")
    }
    recievedGainUpdates ++= gainSignals

    val stateSignals = mostRecentSignalMap filter { _._2.isRight } map {
      case (id, Right(st)) => (id, st)
      case otherwise => throw new Error("Right expected")
    }
    recievedStateUpdates ++= stateSignals

    phase match {
      case MGMPhase.maxGainExchange => {
        // Change phase
        phase = stateChangeInform

        if (stepCounter > 450) {}

        recievedGainUpdates += id -> computedMaxGain.map(_.gain).getOrElse(
          throw new Exception("No computed max gain"))

        val gains = recievedGainUpdates.toList

        // All maximum gains
        val maxGains = Util.maxValuesBy(gains) {
          Ordering[Gain].on(_._2)
        }

        // Break ties by id
        val maxGain = Util.maxValuesBy(maxGains) { Ordering[Int].on(_._1.asInstanceOf[Int]) }

        // If there isn't exactly one signal chosen, something is wrong!
        // Should be save to remove though. Tests were successful
        if (maxGain.size != 1) {
          throw new Exception(
            "[error] List of maximum gains has size ${maxGain.size} instead of zero!" +
              " Either no signals were recieved or ids are not unique!")
        }

        // If the chosen id is this vertex' id it either can achieve highest gain
        // or the vertex is among the ones with the highest gain and it was chosen
        // by having the highest id among them.
        if (maxGain.head._1 == id) {
          // Accept new state
          computedMaxGain map {_.state} getOrElse (throw new Exception("No computed max gain"))
        } else {
          state
        }
      }
      case MGMPhase.stateChangeInform => {
        // Determine which values in the domain should be considered
        val prospStates: Seq[Int] = prospectiveStates(domain.toSet)

        // Calculate the scores for each of these values.
        val evaluatedStates = evaluateAll(prospStates.toSeq)

        // Choose the score according to a certain decision rule.
        val evaluatedCurrentState = evaluate(state)
        val chosenState = decisionRule(evaluatedCurrentState)(evaluatedStates)

        // Save the max gain for later submission
        val (st, payoff) = evaluate(chosenState)

        val gain = payoff - evaluatedCurrentState._2

        // Refresh max state and its associated gain for later transmission
        computedMaxGain = Some(MaxGain(gain, st))

        // Change phase
        phase = maxGainExchange

        state
      }
    }
  }

  override def scoreSignal = {
    1
  }
}


class MGMVertexBuilder(
    randomInitialState: Boolean)
  extends VertexBuilder {
  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new MGMVertex(id, initialState, domain)
  }

  override def toString = s"MGM"
}


class MGMEdge[TargetId](targetId: TargetId)
  extends DefaultEdge(targetId) {

  def signal: Signal = {
    source.asInstanceOf[MGMVertex].phase match {
      case MGMPhase.maxGainExchange => {
        val maxGain =  source.asInstanceOf[MGMVertex].computedMaxGain
        maxGain match {
          case Some(MaxGain(gain, st)) => Left(gain)
          case None => throw new Exception("no max gain computed")
        }
      }
      case MGMPhase.stateChangeInform => {
        Right(source.asInstanceOf[MGMVertex].state)
      }
    }
  }
}
