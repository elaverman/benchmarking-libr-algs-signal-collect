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

import com.signalcollect.dcopthesis.{VertexBuilder, ColorConstrainedVertex}
import com.signalcollect.dcopthesis.libra.components._
import scala.util.Random
import com.signalcollect.Vertex

/**
 * Created with IntelliJ IDEA.
 * User: robin
 * Date: 6/9/13
 * Time: 11:44 AM
 * To change this template use File | Settings | File Templates.
 */
class JSFPIVertex(
    newId: Int,
    initialState: Int,
    newDomain: Array[Int],
    val inertia: Double)
  extends ColorConstrainedVertex[Int,Int](newId, initialState, newDomain)
  with ArgmaxBIDecision[Int]
  with CompleteSearch[Int]
  with JointFictiousPlayTarget[Int]
  with FloodSchedule

class JSFPIVertexBuilder(
    randomInitialState: Boolean,
    inertia: Double)
  extends VertexBuilder {
  def apply(id: Int, domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val initialState = if (randomInitialState) domain(r.nextInt(domain.size)) else domain.head
    new JSFPIVertex(id, initialState, domain, inertia)
  }

  override def toString = s"JSFPI I=$inertia"
}
