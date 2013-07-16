package com.signalcollect.dcopthesis

import org.scalatest.{FunSpec, FunSuite}
import com.signalcollect.dcopthesis.libra.components.Average

/**
 * Created with IntelliJ IDEA.
 * User: robin
 * Date: 6/11/13
 * Time: 9:43 AM
 * To change this template use File | Settings | File Templates.
 */
class AverageTest extends FunSpec {

  info("The average holder class")

  describe("Average") {

    it("should correctly update averages") {
      val x = Average.create(1).updated(2).updated(3)
      val y = Average.create(0).updated(0).updated(0).updated(1)
      assert(x.value == 2)
      assert(y.value == 0.25)
    }

    it("should be creatable as empty without influencing the average") {
      val x = Average.empty.updated(1).updated(2).updated(3)
      assert(x.value == 2)
    }
  }
}
