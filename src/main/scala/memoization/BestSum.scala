package memoization

import org.apache.log4j.Logger

import scala.collection.mutable

object BestSum extends App {

  /**
   * give and array and a target sum
   * - can any combination of the array sum to target
   * - combination includes also self
   * - sum always bigger than zero
   * - element in array can be use as many times as needed
   * - nums always positive
   * - return shortest combination
   */

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = false
  var level = 0

  /**
   *
   * @param array
   * @param target
   * @return
   */
  //m=target sum (assuming worst case scenario of -1)
  //m will be the depth of the tree
  //O(n^m * (m)) time
  //O(m^m) space - m nodes, n splits, n list in each split of size m
  def bestSum(array: Array[Int], target: Int): List[Int] = target match {
    case 0 => Nil
    case x if x < 0 => null
    case _ => {
      val possibleAns = for (e <- array) yield {
        val newTarget = target - e
        bestSum(array, newTarget) match {
          case null => List(-99)
          case Nil => e +: Nil
          case l@List(_*) => e +: l
        }
      }
      if (canlog) logger.info(s"[possible-answers] ${possibleAns.toList}")
      level += 1
      val ans = possibleAns
        .map(e => (e, e.size))
        .foldLeft((possibleAns.head, possibleAns.head.size))(
          (a, b) =>
            if (a._1.contains(-99)) b
            else if (b._1.contains(-99)) a
            else if (a._2 > b._2) b else a
        )
        ._1
      if (canlog) logger.info(s"[answer] at level $level answer: $ans")
      if (ans.contains(-99)) null
      else ans
    }
  }

  //m=target sum (assuming worst case scenario of -1)
  //m will be the depth of the tree
  //O(n^m * (m)) time
  //O(m^m * n) space - m nodes, n splits, n list in each split of size m
  def bestSumOptimize(array: Array[Int], target: Int, cache: mutable.HashMap[Int, List[Int]]): List[Int] = target match {
    case 0 => Nil
    case x if x < 0 => null
    case _ => {
      if (cache.contains(target)) cache(target)
      else {
        val possibleAns = for (e <- array) yield {
          val newTarget = target - e
          bestSumOptimize(array, newTarget, cache) match {
            case null => List(-99)
            case Nil => e +: Nil
            case l@List(_*) => e +: l
          }
        }
        if (canlog) logger.info(s"[possible-answers] ${possibleAns.toList}")
        level += 1
        val ans = possibleAns
          .map(e => (e, e.size))
          .foldLeft((possibleAns.head, possibleAns.head.size))(
            (a, b) =>
              if (a._1.contains(-99)) b
              else if (b._1.contains(-99)) a
              else if (a._2 > b._2) b else a
          )
          ._1
        if (canlog) logger.info(s"[answer] at level $level answer: $ans")
        if (ans.contains(-99)) cache(target) = null
        else cache(target) = ans
        if (canlog) logger.info(s"[current-cache] at level $level answer: $cache")
        cache(target)
      }
    }
  }


  //m=target sum (assuming worst case scenario of -1)
  //m will be the depth of the tree
  //O(n^m * (m)) time
  //O(m^m) space - m nodes, n splits, n list in each split of size m
  def bestSumNoYield(array: Array[Int], target: Int): List[Int] = {
    var shortestComb: List[Int] = null
    target match {
      case 0 => Nil
      case x if x < 0 => null
      case _ =>
        for (e <- array) {
          val newTarget = target - e
          bestSumNoYield(array, newTarget) match {
            case null =>
            case l@List(_*) =>
              val currentComb = e +: l
              if (canlog) logger.info(s"[checking list] $currentComb with $currentComb")
              if (shortestComb == null || (currentComb.size < shortestComb.size)) {
                if (canlog) logger.info(s"[updating-comb] with $currentComb")
                shortestComb = currentComb
              }
          }
        }
        shortestComb
    }
  }


  val testSuite = List(
    (Array(5, 3, 4, 7), 7),
    (Array(2, 3, 5), 8),
    (Array(4), 8),
    (Array(1, 4, 5), 8),
    //(Array(1, 2, 5, 25), 100),
  )

  testSuite.foreach {
    case (data, target) =>
      //val bestsum = timer(bestSum(data, target), "bestsum", logger)
      //logger.info(s"[bestsum] ${data.toList}, target: $target bestsum: $bestsum")
      //val bestsumOptim = timer(bestSumOptimize(data, target, mutable.HashMap.empty), "bestsumOptim", logger)
      //logger.info(s"[bestsumOptimize] ${data.toList}, target: $target bestsum: $bestsumOptim")
      val bestsumNoYiedlWrapp = timer(bestSumNoYield(data, target), "bestsumNoYieldWrap", logger)
      logger.info(s"[bestsum-no-yield] ${data.toList}, target: $target bestsum: $bestsumNoYiedlWrapp")
  }

}
