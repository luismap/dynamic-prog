import Utils.timer
import org.apache.log4j.Logger

import scala.collection.mutable

object FibDynamic extends App {
  val logger = Logger.getLogger(this.getClass.getName)
  //O(n**2) time
  //O(n) space
  def fib(n: Double): Double = {
    if (n <= 2) 1
    else fib(n-1) + fib(n - 2)
  }

  val memoized = mutable.HashMap.empty[Double, Double]
  memoized(1) = 1
  memoized(2) = 1
  //O(n) time
  //O(n) space
  def fibDynamic(n: Double ): Double = {
    if(memoized.contains(n))  memoized(n)
    else {
      memoized(n) = fibDynamic(n-1) + fibDynamic(n-2)
      memoized(n)
    }
  }
  val n = 45 //over 52 will fib exponential take a long time
  val fibN2 = timer(fib(n),"fib_square",logger)
  val fibN = timer(fibDynamic(n), "fib_lineal", logger)


 logger.info(s"[fib] fib of $fibN2")
  logger.info(s"[fib] fib of $fibN")

}
