package leet
import org.apache.log4j.Logger

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
object LongestSubstring extends App {

  val log = Logger.getLogger(this.getClass.getName)
  val canlog = true

  def lengthOfLongestSubstring(s: String): Int = {
    var ans = 1
    val l = s.length
    var tmpString = new StringBuilder
    if (l == 0) return 0
    if (l == 1) return 1
    else {
      ans = 1
      for (idx <- 0 until l) {
        tmpString = new StringBuilder
        var steps = 0
        breakable {
          //get only substr bigger than current ans
          for (bidx <- idx to idx + ans) {
            if (bidx >= l) break
            if (tmpString.contains(s(bidx))) break
            else {
              tmpString += s(bidx)
              steps += 1
            }
          }
          ans = steps
          if(canlog) log.info(s"working string beginning with $tmpString current idx ${idx + steps}")
          for (nidx <- idx + steps until l){
            if(canlog) log.info(s"does $tmpString contains ${s(nidx)}")
            if(tmpString.contains(s(nidx))) break
            else {
              tmpString +=s(nidx)
              ans = tmpString.size
              if(canlog) log.info(s"current longest substring $tmpString with size $ans ")
            }
          }
        }
      }
    return ans
    }
  }

  val testSuite = Array(
    ("pwwkew", 3),
    ("abcabcbb",3),
    ("bbbbb",1),
    ("au",1)
  )

  for (e <- testSuite) {
    val ans = lengthOfLongestSubstring(e._1)
    log.info(s"[longest String] for ${e._1} has length $ans")
  }

}
