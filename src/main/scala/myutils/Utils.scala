package myutils

import org.apache.log4j.Logger

object Utils {

  def timer[A](f: => A, name: String, logger: Logger): A = {
    val init = System.currentTimeMillis()
    val x = f
    val end = System.currentTimeMillis()
    logger.info(s"[total time] for $name = ${(end - init) / 1000} seconds")
    x
  }
}
