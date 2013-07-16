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

package com.signalcollect.dcopthesis.libra
import com.signalcollect._
import java.util.Random
import com.signalcollect.dcopthesis.{VertexBuilder, ColorConstrainedVertex}
import DSANVertex.{TimeStep, UtilityDelta, Probability}
import com.signalcollect.dcopthesis.libra.components.{SimulatedAnnealingDecision, MapUtilityTarget, CompleteSearch, ParallelRandomSchedule}
import com.signalcollect.dcopthesis.libra.components.RandomStateSearch

// Simple Type synonyms
object DSANVertex {
  type TimeStep = Int
  type UtilityDelta = Double
  type Probability = Double
}

class DSANVertexBuilder(
    randomInitialState: Boolean,
    pSchedule: Double,
    const: Int,
    k: Int = 2)
  extends VertexBuilder {

  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val initialState = {
      if (randomInitialState) { 
        val r = new Random
        domain(r.nextInt(domain.size))
      }
      else { 
        domain(0)
      }
    }

    new DSANVertex(id, initialState, domain, pSchedule, const, k)
  }

  override def toString = s"DSAN p=$pSchedule c=$const"
}

/**
 * Implementation of Distributed Simulated Annealing
 * ( Arshad, Silaghi, 2003. "Distributed Simulated Annealing and comparison to DSA".
 *  In Proceedings of the 4th International Workshop on Distributed Contraint Reasoning, Acapulco, Mexico)
 * The collect function chooses a new random state and chooses it if it improves over the old state,
 * or, if it doesn't it still chooses it (for exploring purposes) with probability decreasing with time
 */
class DSANVertex(id: Int, initialState: Int, newDomain: Array[Int], val p: Double, val const: Int, val k: Int)
  extends ColorConstrainedVertex[Int,Int](id, initialState, newDomain)
  with SimulatedAnnealingDecision[Int]
  with RandomStateSearch[Int]
  with MapUtilityTarget[Int]
  with ParallelRandomSchedule
