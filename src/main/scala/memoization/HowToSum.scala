package memoization

import org.apache.log4j.Logger

import scala.collection.mutable

object HowToSum extends App {

  /**
   * give and array and a target sum
   * - can any combination of the array sum to target
   * - combination includes also self
   * - sum always bigger than zero
   * - element in array can be use as many times as needed
   * - nums always positive
   * return the possible path to solution
   */

  val canLog = false
  val logger = Logger.getLogger(this.getClass.getName)

  def canSumWrapper(array: Array[Int], target: Int): List[Int] = {
    var counter = 0

    //m worst case scenario tree depth
    //n size of array
    //O(n^m * m) time
    //O(m) space
    def howToSum(arr: Array[Int], target: Int): List[Int] = {
      if (target == 0) Nil
      else if (target < 0) null
      else {
        val possibilities = for (e <- arr) yield {
          val newTarget = target - e
          howToSum(arr, newTarget) match {
            case null => null
            case Nil => e +: Nil
            case l@List(_, _*) => e +: l
          }
        }
        counter += 1
        if (canLog) logger.info(s"[currentlevel] target = $target edges = ${possibilities.toList}")
        val ans = possibilities
          .find(_.isInstanceOf[List[Int]])
          .getOrElse(null)
          .asInstanceOf[List[Int]]
        if (canLog) logger.info(s"[current-ans] $ans at level $counter")

        ans
      }
    }

    howToSum(array, target)
  }

  //m worst case scenario tree depth
  //n size of array
  //O(n**m * m) time
  //O(m) space
  def howToSumFunctional(arr: Array[Int], target: Int): List[Int] = target match {
    case 0 => Nil
    case x if x < 0 => null
    case _ =>
      val ans = for (e <- arr) yield {
        val newTarget = target - e
        howToSumFunctional(arr, newTarget) match {
          case null => null
          case Nil => e +: Nil
          case l@List(_*) => e +: l

        }
      }
      if (canLog) logger.info(s"[currentlevel] target = $target edges = ${ans.toList}")
      ans.find(_.isInstanceOf[List[Int]])
        .getOrElse(null)
        .asInstanceOf[List[Int]]
  }

  //m worst case scenario tree depth
  //n size of array
  //O(n * m^2) time - plus operation of prepending to list 0(1), but at most m prepending
  //O(m * m) space - store m keys plus at most m elements
  def howToSumFunctionalOptimize(arr: Array[Int], target: Int, cache: mutable.HashMap[Int, List[Int]]): List[Int] = target match {
    case 0 => Nil
    case x if x < 0 => null
    case _ =>
      if (cache.contains(target)) cache(target)
      else {
        val ans = for (e <- arr) yield {
          val newTarget = target - e
          howToSumFunctionalOptimize(arr, newTarget, cache) match {
            case null =>
              if (!cache.contains(target)) cache(target) = null
            case Nil =>
              cache(target) = e +: Nil
              if (canLog) cache(target)
            case l@List(_*) =>
              cache(target) = e +: l
              if (canLog) cache(target)
          }
        }
        if (canLog) logger.info(s"[currentlevel] target = $target edges = ${ans.toList}")
        if (canLog) logger.info(s"[cache] $cache")
        cache.getOrElse(target, null)
      }

  }


  val testSuite = List(
    (Array(1, 1), 3),
    (Array(2, 3), 7),
    (Array(5, 3, 4, 7), 7)
    , (Array(2, 4), 7),
    (Array(2, 3, 5), 8)
    , (Array(7, 14), 300)
    //,(Array(10, 14), 300)
  )

  testSuite.foreach {
    case (data, target) =>
      val howtosum = timer(howToSumFunctional(data, target), "howToSumFunc", logger)
      logger.info(s"[howTosumFunc] ${data.toList}, target: $target cansum: $howtosum")
      val howtosumOptim = timer(howToSumFunctionalOptimize(data, target, mutable.HashMap.empty), "howToSumFuncOpti", logger)
      logger.info(s"[how-to-sum-optimize] ${data.toList}, target: $target cansum: $howtosumOptim")
  }

}
