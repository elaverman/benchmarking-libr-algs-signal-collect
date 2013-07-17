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

/**
 * Spatial Adaptive Play
 * 
 * The probability for each state is given by the Boltzmann distribution:
 * P_s(η) = e^(1/η * u(s))/∑(e^(1/η * u(s_i)))
 * where u(si) = utility(state i)
 * η = temperature parameter
 * target function: immediate payoff
 * decision rule: boltzmann distribution
 * adjusment schedule: random sequential
 */


package com.signalcollect.dcopthesis.libra


import com.signalcollect.dcopthesis.{VertexBuilder, ColorConstrainedVertex}
import com.signalcollect.Vertex
import scala.util.Random
import com.signalcollect.dcopthesis.libra.components._


abstract class SAPVertex(
    newId: Int,
    initialState: Int,
    newDomain: Array[Int],
    var η: Double,
    val ηDecrement: Double = 0.0)
  extends ColorConstrainedVertex[Int, Int](newId, initialState, newDomain)
  with CompleteSearch[Int]
  with MapUtilityTarget[Int]
  with LogisticDecision[Int]
  with SequentialRandomSchedule


class SAPVertexBuilder(
    randomInitialState: Boolean,
    ηInitial: Double,
    vertexIdsSchedule: Seq[Int],
    ηDecrement: Double = 0.0)
  extends VertexBuilder {
  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new SAPVertex(id, initialState, domain, ηInitial, ηDecrement ) {
      val vertexIds = vertexIdsSchedule
    }
  }

  override def toString = s"SAP eta=$ηInitial"
}
