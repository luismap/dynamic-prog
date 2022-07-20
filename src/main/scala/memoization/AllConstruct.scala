package memoization

import org.apache.log4j.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AllConstruct extends App {

  /**
   * given an array of string(wordbank) and a targe string
   *  - return all ways target can be constructed
   *  - by concatenating elements of the wordbank
   *  - may reuse elements of wordbank as needed
   */

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = false

  //n = array size
  //m = target size/tree depth
  //time: O((n^m) * m )
  //space: O(m*m)
  def allConstruct(array: Array[String], target: String): ListBuffer[ListBuffer[String]] = {
    val ans: ListBuffer[ListBuffer[String]] = ListBuffer.empty[ListBuffer[String]]
    target match {
      case "" => ListBuffer.empty
      case str =>
        for (e <- array) {
          val (prefix, newTarget) = str.splitAt(e.length)
          if (canlog) logger.info(s"[checking] prefix $prefix elem: $e parent: $target newTarget $newTarget")
          if (prefix == e) {
            if (canlog) logger.info(s"[status] parent:$target prefix: $prefix newTarget: $newTarget ")
            allConstruct(array, newTarget) match {
              case null =>
              case l if l == Nil =>
                ans += ListBuffer(prefix)
                if (canlog) logger.info(s"[partial] answer $ans")
              case l@ListBuffer(_*) =>
                if (canlog) logger.info(s"[partial] answer $l")
                ans ++= l.map(e => prefix +=: e)

            }
          }
        }
        if (ans.isEmpty) null
        else ans
    }
  }

  //n = array size
  //m = target size/tree depth
  //time: O((n^m) * m )
  //space: O(m*m)
  def allConstructOptim(array: Array[String], target: String, cache: mutable.HashMap[String, ListBuffer[ListBuffer[String]]]): ListBuffer[ListBuffer[String]] = {
    val ans: ListBuffer[ListBuffer[String]] = ListBuffer.empty[ListBuffer[String]]
    target match {
      case "" => ListBuffer.empty
      case str =>
        if (cache.contains(target)) cache(target)
        else {
          for (e <- array) {
            val (prefix, newTarget) = str.splitAt(e.length)
            if (canlog) logger.info(s"[checking] prefix $prefix elem: $e parent: $target newTarget $newTarget")
            if (prefix == e) {
              if (canlog) logger.info(s"[status] parent:$target prefix: $prefix newTarget: $newTarget ")
              allConstructOptim(array, newTarget, cache) match {
                case null =>
                case l if l == Nil =>
                  cache(target) = ans += ListBuffer(prefix)
                  if (canlog) logger.info(s"[partial] answer $ans")
                case l@ListBuffer(_*) =>
                  if (canlog) logger.info(s"[partial] answer $l")
                  cache(target) = ans ++= l.map(e => prefix +=: e)

              }
            }
          }
        }
        cache.getOrElse(target, null)
    }
  }

  val testSuite = List(
    (Array("ka", "ka"), "ka"),
    (Array("ska", "a", "pe", "ka", "s"), "ska"),
    (Array("ab", "abc", "cd", "def", "abcd", "ef", "c"), "abcdef"),
    (Array("bo", "rd", "ate", "t", "ska", "sk", "boar"), "skateboard"),
    (Array("purp", "p", "ur", "le", "purpl"), "purple"),
    (Array("purpl"), "purple"),
    (Array("a", "p", "ent", "enter", "ot", "o", "t"), "enterapotentpot"),
    (Array("e", "ee", "eee", "eeee", "eeeee", "eeeeeee"), "eeeeeeeeeeeeeeeeeeeeeeeeeeeeef"),
  )

  testSuite.foreach {
    case (data, target) =>
      val all_construct = timer(allConstruct(data, target), "all_construct", logger)
      logger.info(s"[all_construct] ${data.toList}, target: $target allconstruct: $all_construct")

      val all_construct_optim = timer(allConstructOptim(data, target, mutable.HashMap.empty), "all_construct_optim", logger)
      logger.info(s"[all_construct] ${data.toList}, target: $target canconstruct: $all_construct")
  }

}
