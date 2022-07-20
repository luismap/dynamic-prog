package memoization

import myutils.Utils.timer
import org.apache.log4j.Logger

import scala.collection.mutable

object CanConstruct extends App {

  /**
   * given an array of string(wordbank) and a targe string
   *  - return boolean whether target can be constructed
   *  - by concatenating elements of the wordbank
   *  - may reuse elements of wordbank as needed
   */

  val canlog = false
  val logger = Logger.getLogger(this.getClass.getName)

  //n = array size
  //m = target size/tree depth
  //time: O((n^m) * m) las m because of array split
  //space: O(m * m)
  def canConstruct(array: Array[String], target: String): Boolean = {
    var currentReturn = false
    target match {
      case "" => currentReturn = true
      case s: String =>
        for (e <- array) {
          val (prefix, newTarget) = s.splitAt(e.length)
          if (prefix == e) {
            if (canlog) logger.info(s"[status-level] parent: $target prefix: $prefix current_element: $e newTarget: $newTarget ")
            currentReturn = canConstruct(array, newTarget)
          }
        }
    }
    currentReturn
  }

  //n = array size
  //m = target size/tree depth
  //time: O(n*m^2)
  //space: O(m^2)
  def canConstructOptimize(array: Array[String], target: String, cache: mutable.HashMap[String, Boolean]): Boolean = {
    var currentReturn = false
    if (cache.contains(target)) cache(target)
    else {
      target match {
        case "" => currentReturn = true
        case s: String =>
          for (e <- array) {
            val (prefix, newTarget) = s.splitAt(e.length)
            if (prefix == e) {
              if (canlog) logger.info(s"[status-level] parent: $target prefix: $prefix current_element: $e newTarget: $newTarget ")
              currentReturn = canConstructOptimize(array, newTarget, cache)
            }
          }
      }
      cache(target) = currentReturn
      cache(target)
    }

  }

  val testSuite = List(
    (Array("a", "pe", "ka", "s"), "ska"),
    (Array("ab", "abc", "cd", "def", "abcd"), "abcdef"),
    (Array("bo", "rd", "ate", "t", "ska", "sk", "boar"), "skateboard"),
    (Array("e", "ee", "eee", "eeee", "eeeee", "eeeeeee"), "eeeeeeeeeeeeeeeeeeeeeeeeeeeeef")
  )

  testSuite.foreach {
    case (data, target) =>
      val can_construct = timer(canConstruct(data, target), "can_construct", logger)
      logger.info(s"[can_construct] ${data.toList}, target: $target canconstruct: $can_construct")
      val can_construct_optimize = timer(canConstructOptimize(data, target, mutable.HashMap.empty), "can_construct_optimize", logger)
      logger.info(s"[can_construct_optimize] ${data.toList}, target: $target canconstruct: $can_construct_optimize")
  }

}
