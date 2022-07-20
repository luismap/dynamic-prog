package memoization

import myutils.Utils.timer
import org.apache.log4j.Logger

import scala.collection.mutable

object CanSum extends App {
  /**
   * give and array and a target sum
   * - can any combination of the array sum to target
   * - combination includes also self
   * - sum always bigger than zero
   * - element in array can be use as many times as needed
   * - nums always positive
   */

  val canLog = false
  val logger = Logger.getLogger(this.getClass.getName)

  //m=target sum (assuming worst case scenario of -1)
  //m will be the depth of the tree
  //n=size of array
  //O(n^m) time
  //O(m) space
  def canSum(array: Array[Int], target: Int): Boolean = {
    if (target == 0) true
    else {
      val e = for (e <- array) yield {
        val newTarget = target - e
        if (newTarget >= 0) {
          if (canLog) logger.info(s"[new target] ${array.toList}, target: $newTarget ")
          canSum(array, newTarget)
        }
      }
      if (canLog) logger.info(s"[currentlevel] ${e.toList} for target: $target")
      if (e.contains(true)) true
      else false
    }

  }


  //O(n * m) time
  //O(m) space
  def canSumOptimize(array: Array[Int], target: Int, cached: mutable.HashMap[Int, Boolean]): Boolean = {
    if (cached.contains(target)) cached(target)
    else if (target == 0) true
    else if (target < 0) false
    else {
      val e = for (e <- array) yield {
        val newTarget = target - e
        if (newTarget >= 0)
          if (canLog) logger.info(s"[new target] ${array.toList}, target: $newTarget ")
        canSumOptimize(array, newTarget, cached)
      }
      if (canLog) logger.info(s"[currentlevel] ${e.toList} for target: $target")
      if (e.contains(true)) {
        cached(target) = true
      } else {
        cached(target) = false
      }
      cached(target)
    }
  }

  val testSuite = List(
    (Array(2, 3), 7),
    (Array(5, 3, 4, 7), 7),
    (Array(2, 4), 7),
    (Array(2, 3, 5), 8)
    , (Array(7, 14), 300)
  )

  testSuite.foreach {
    case (data, target) =>
      val cansum = timer(canSum(data, target), "cansum", logger)
      val cansumOptimize = timer(canSumOptimize(data, target, mutable.HashMap.empty), "cansumOptimize", logger)
      logger.info(s"[cansum] ${data.toList}, target: $target cansum: $cansum")
      logger.info(s"[cansumOptimize] ${data.toList}, target: $target cansum: $cansumOptimize")
  }

}
