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
 * The distributed version of tabu search
 */


package com.signalcollect.dcopthesis.libra


import com.signalcollect.dcopthesis.{VertexBuilder, ColorConstrainedVertex}
import com.signalcollect.dcopthesis.libra.components._
import com.signalcollect.Vertex
import scala.util.Random


class TabuSearchVertex(
    newId: Int,
    initialState: Int,
    newDomain: Array[Int],
    val p: Double,
    val stepsToRemember: Int)
  extends ColorConstrainedVertex[Int,Int](newId, initialState, newDomain)
  with CompleteSearch[Int]
  with TabuListDecision[Int] 
  with MapUtilityTarget[Int]
  with ParallelRandomSchedule

  
class TabuSearchVertexBuilder(
    randomInitialState: Boolean,
    pSchedule: Double,
    stepsToRemember: Int)
  extends VertexBuilder {

  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new TabuSearchVertex(id, initialState, domain, pSchedule, stepsToRemember)
  }

  override def toString = s"TS p=$pSchedule L=$stepsToRemember"
}
