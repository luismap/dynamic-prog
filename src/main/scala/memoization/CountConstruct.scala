package memoization

import org.apache.log4j.Logger

import scala.collection.mutable

object CountConstruct extends App {

  /**
   * given an array of string(wordbank) and a targe string
   *  - return count of how many ways target can be constructed
   *  - by concatenating elements of the wordbank
   *  - may reuse elements of wordbank as needed
   */

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = false

  //n = array size
  //m = target size/tree depth
  //time: O((n^m) * m )
  //space: O(m*m)
  def countConstruct(array: Array[String], target: String): Int = {
    var ans = 0
    target match {
      case "" => ans = 1
      case s =>
        for (e <- array) {
          val (prefix, newTarget) = s.splitAt(e.length)
          if (prefix == e) {
            if (canlog) logger.info(s"[status] parent:$target prefix: $prefix newTarget: $newTarget ")
            ans += countConstruct(array, newTarget)
          }
        }
    }
    ans
  }

  //n = array size
  //m = target size/tree depth
  //time: O((n * m) * m )
  //space: O(m*m)
  def countConstructOptimize(array: Array[String], target: String, cache: mutable.HashMap[String, Int]): Int = {
    var ans = 0
    if (cache.contains(target)) cache(target)
    else {
      target match {
        case "" => ans = 1
        case s =>
          for (e <- array) {
            val (prefix, newTarget) = s.splitAt(e.length)
            if (prefix == e) {
              if (canlog) logger.info(s"[status] parent:$target prefix: $prefix newTarget: $newTarget ")
              ans += countConstructOptimize(array, newTarget, cache)
            }
          }
      }
      cache(target) = ans
      cache(target)
    }
  }

  val testSuite = List(
    (Array("a", "pe", "ka", "s"), "ska"),
    (Array("ab", "abc", "cd", "def", "abcd"), "abcdef"),
    (Array("bo", "rd", "ate", "t", "ska", "sk", "boar"), "skateboard"),
    (Array("purp", "p", "ur", "le", "purpl"), "purple"),
    (Array("a", "p", "ent", "enter", "ot", "o", "t"), "enterapotentpot"),
    (Array("e", "ee", "eee", "eeee", "eeeee", "eeeeeee"), "eeeeeeeeeeeeeeeeeeeeeeeeeeeeef"),
  )

  testSuite.foreach {
    case (data, target) =>
      val count_construct = timer(countConstruct(data, target), "can_construct", logger)
      logger.info(s"[can_construct] ${data.toList}, target: $target canconstruct: $count_construct")
      val count_construct_optim = timer(countConstructOptimize(data, target, mutable.HashMap.empty), "can_construct_optim", logger)
      logger.info(s"[can_construct_optim] ${data.toList}, target: $target canconstruct: $count_construct_optim")
  }


}
