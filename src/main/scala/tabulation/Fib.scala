package tabulation

import myutils.Utils.timer
import org.apache.log4j.Logger

import scala.collection.mutable.ArrayBuffer

object Fib extends App {

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = true

  /**
   * tabulation.
   * - use a table to represent solution
   * - init table(in our case an array)
   * - init base cases
   */

  //O(n) time
  //O(n) space
  def fibTab(n: Int): Int = {
    val array = new Array[Int](n + 2) //init table
    if (canlog) logger.info(s"Array ${array.toList}")
    array(1) = 1 //init base case
    for (idx <- 0 until  n) {
      array(idx + 1) += array(idx)
      array(idx + 2) += array(idx)
      if (canlog) logger.info(s"working indx $idx ${array.toList}")
    }

    if (canlog) logger.info(s"Array ${array.toList}")
    array(n)
  }

  val n = 6
  val fibN = timer(fibTab(n), "fib_lineal", logger)


  logger.info(s"[fib] fib of $n = $fibN ")
}
