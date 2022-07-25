package leet

import org.apache.log4j.Logger


object TwoSum extends App {

  import util.control.Breaks._

  val log = Logger.getLogger(this.getClass.getName)
  val canlog = false

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val cache = collection.mutable.HashMap.empty[Int, Int]
    for (idx <- nums.indices) {
      val ce = nums(idx)
      val newTarget = target - ce
      if (canlog) log.info(s"working-index $idx")
      if (cache.contains(ce)) {
        return Array(cache(ce), idx) //this return will always be reached
      }
      else {
        if (canlog) log.info(s"adding to cache $newTarget -> $idx")
        cache(newTarget) = idx
      }
    }

    log.info(s"it matters not, you will not reach here, just for compiler")
    Array(1)
  }




  val testSuite = Array(
    (Array(2, 7, 11, 15), 9),
    (Array(3, 2, 4), 6),
    (Array(3, 3), 6),
  )

  for (e <- testSuite) {
    val ans = twoSum(e._1, e._2)
    log.info(s"[twoSum] data ${e._1.toList} target ${e._2} = ${ans.toList}")
  }

}
