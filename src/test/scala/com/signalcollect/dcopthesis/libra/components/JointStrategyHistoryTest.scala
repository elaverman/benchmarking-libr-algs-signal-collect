package com.signalcollect.dcopthesis.libra.components

import org.scalatest.{FunSpec, BeforeAndAfter, FunSuite}

/**
 * Created with IntelliJ IDEA.
 * User: robin
 * Date: 6/13/13
 * Time: 11:52 AM
 * To change this template use File | Settings | File Templates.
 */
class JointStrategyHistoryTest extends FunSpec with BeforeAndAfter {

  val neighbourIds = Seq(1,2,3,4)
  val domain = Seq(10,11,12,13)
  var strat: JointStrategyHistory[Int] = _

  before {
    strat = new JointStrategyHistory[Int](neighbourIds, domain)
  }

  describe("JointStrategyHistory") {
    it("should be initialized with uniform probabilities") {
      strat.neighbourhoods foreach { c =>
        assert(strat.get(c).isDefined)
        assert(strat.get(c).exists(_ == 1.0 / strat.neighbourhoods.size))
      }
    }

    it("should sum up to approx 1") {
        var sum = 0.0
        strat.neighbourhoods foreach { c =>
          sum += strat.get(c).getOrElse(1000.0)
      }
      assert(0.999 < sum && sum < 1.001 )
    }

    it("should sum up to approx 1 after countless updates") {
      var sum = 0.0
      strat.neighbourhoods foreach { c =>
        strat.update(c)
      }
      strat.neighbourhoods foreach { c =>
        sum += strat.get(c).getOrElse(1000.0)
      }
      assert(0.999 < sum && sum < 1.001 )
    }

    it("should throw an exception when trying to update with a invalid key") {
      intercept[Exception] {
        strat.update(Seq(1,2))
      }
    }
  }
}
