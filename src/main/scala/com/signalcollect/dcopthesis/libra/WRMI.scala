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
 * Weighted regret matching with inertia
 */


package com.signalcollect.dcopthesis.libra


import com.signalcollect.dcopthesis._
import com.signalcollect.Vertex
import scala.util.Random
import com.signalcollect.dcopthesis.libra.components._


abstract class WRMIVertex(newId: Int, newState: Int, newDomain: Array[Int], val inertia: Double)
  extends ColorConstrainedVertex[Int,Int](newId, newState, newDomain)
  with FloodSchedule
  with DiscountedRegretTarget[Int]
  with CompleteSearch[Int]
  with ArgmaxBIDecision[Int]


class WRMIVertexBuilder(
    randomInitialState: Boolean,
    inertia: Double,
    memory: Double)
  extends VertexBuilder {
  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new WRMIVertex(id, initialState, domain, inertia) {
      val fadingMemory = memory
    }
  }

  override def toString = s"WRMI I=$inertia, M=$memory"
}
