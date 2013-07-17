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
 *
 */


/*
 * Fictitious play -- not used in the benchmark since JSFP-I represents the same family
 * 
 * The payoff for changing to a certain state depends on the historic frequencies of neighboring states.
 * Vertex i's payoff of acquiring state si at time t is given by the SUM over all neighbors of their
 * probability not to be in state si times the utility the vertex would gain with state si.
 * 
 * Based on a description from:
 *   Chapman, A. C., Rogers, A., Jennings, N. R., and Leslie, D. S. (2011b).
 *   A unifying framework for iterative approximate best-response algorithms
 *   for distributed constraint optimization problems.
 */

package com.signalcollect.dcopthesis.libra


import com.signalcollect.dcopthesis._
import com.signalcollect.Vertex
import scala.util.Random
import com.signalcollect.dcopthesis.libra.components._


class FPVertex(
  newId: Int,
  initialState: Int,
  newDomain: Array[Int],
  val inertia: Double)
  extends ColorConstrainedVertex[Int, Int](newId, initialState, newDomain)

  with ArgmaxBIDecision[Int]
  with CompleteSearch[Int]
  with FictiousPlayTarget[Int]
  with FloodSchedule {

  type Signal = Int
}

class FPVertexBuilder(
  randomInitialState: Boolean,
  inertia: Double)
  extends VertexBuilder {

  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new FPVertex(id, initialState, domain, inertia)
  }

  override def toString = s"FP[i=$inertia]"
}
