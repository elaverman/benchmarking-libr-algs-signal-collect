package com.signalcollect.dcopthesis

import scala.annotation.tailrec

/**
 * An object providing utility functions that by be used by multiple algorithms.
 */
object Util {
  /**
   * A function which returns all maximum values of a container.
   * @param elems The list for which to extract the maximum elements
   * @tparam T A type for which an ordering exists
   * @return The maximal values
   */
  // Come to realize later:
  // val max = elems.max; elems filter { _ == max }
  // would have done the trick as well :S
  def maxValuesBy[T : Ordering](elems: Iterable[T]): Iterable[T] = {
    @tailrec
    def maxValues(xss: List[T], currentMax: List[T]): List[T] = xss match {
      case Nil => currentMax
      case (x::xs) => currentMax match {
        case Nil => maxValues(xs, List(x)) // No current max, take the first element as the new max
        case maxs@(max::_) => {
          val cmp = implicitly[Ordering[T]].compare(x, max)
          if (cmp < 0) {
            maxValues(xs, maxs)
          } else if (cmp == 0) {
            maxValues(xs, x::maxs)
          } else {
            maxValues(xs, List(x))
          }
        }
      }
    }
    maxValues(elems.toList, List())
  }

  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }
}
