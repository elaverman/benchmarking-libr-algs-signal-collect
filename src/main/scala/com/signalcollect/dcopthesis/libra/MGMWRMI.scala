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

/*
 * maximum-gain weighted regret matching with inertia
 */


package com.signalcollect.dcopthesis.libra


import com.signalcollect.dcopthesis.{VertexBuilder}
import com.signalcollect.dcopthesis.libra.components._
import com.signalcollect.Vertex
import scala.util.Random


class MGMWRMIVertex(
    newId: Int,
    initialState: Int,
    newDomain: Array[Int],
    val fadingMemory: Double)
  extends MGMVertex(newId, initialState, newDomain)
  with ArgmaxBDecision[Int]
  with DiscountedRegretTarget[Int]
  with CompleteSearch[Int]
  with FloodSchedule

  
class MGMWRMIVertexBuilder(
    randomInitialState: Boolean,
    fadingMemory: Double)
  extends VertexBuilder {
  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new MGMWRMIVertex(id, initialState, domain, fadingMemory)
  }

  override def toString = s"MGM-WRM M=$fadingMemory"
}
