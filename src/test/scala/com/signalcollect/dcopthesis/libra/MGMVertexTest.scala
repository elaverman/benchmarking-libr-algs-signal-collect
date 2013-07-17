package com.signalcollect.dcopthesis.libra

import org.scalatest.{BeforeAndAfter, FunSpec}
import com.signalcollect.dcopthesis.libra.MGMVertex.MGMSignal

/**
 * Created with IntelliJ IDEA.
 * User: robin
 * Date: 5/29/13
 * Time: 4:57 PM
 * To change this template use File | Settings | File Templates.
 */
class MGMVertexTest extends FunSpec with BeforeAndAfter {

  var mgmSignals: Iterable[(Int, MGMSignal)] = _
  var gainOrd: Ordering[(Int,MGMSignal)] = _

  before {
    mgmSignals = Iterable(
      (0, Left(1.0)),
      (1, Left(2.0)),
      (2, Left(2.0)),
      (3, Left(3.0)),
      (4, Left(5.0)),
      (5, Left(5.0)),
      (6, Left(2.0)),
      (7, Left(1.0)))

      gainOrd = new Ordering[(Int,MGMSignal)] {
        def compare(x: (Int,MGMSignal), y: (Int,MGMSignal)): Int = {
          (x._2, y._2) match {
            case (Left(xx), Left(yy)) => Ordering[Double].compare(xx,yy)
            case otherwise => throw new Exception("Left expected")
          }
        }
      }

  }

  info("maxValueBy on MGMSignals")

  describe("maxValueBy") {
    it("should return all values with the highest value") {
      val maxGains = Util.maxValuesBy(mgmSignals)(gainOrd)
      assert(maxGains.size == 2)
      assert(maxGains.map(_._2 == Left(5.0)).foldLeft(true)( _ && _ ))
    }
    it("should return the MGMSignal with the highest gain, breaking ties between ids") {
      val maxGains = Util.maxValuesBy(mgmSignals)(gainOrd)
      val maxGain = Util.maxValuesBy(maxGains)(Ordering[Int].on(_._1))
      assert(maxGain.head._1 == 5)
      assert(maxGain.size == 1)
    }
  }

}
