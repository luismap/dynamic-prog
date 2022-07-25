package leet.linklist

import org.apache.log4j.Logger

object AddTwoNumbers extends App {

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = true

  case class ListNode(var x: Int = 0, var next: ListNode = null)

  object Solution {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      var currentNode = ListNode()
      val ans = currentNode
      var currentL1 = l1
      var currentL2 = l2
      var carryOver = 0
      while (currentL1 != null || currentL2 != null || carryOver != 0) {
        val left = if(currentL1 != null) currentL1.x else 0
        val right = if(currentL2 != null) currentL2.x else 0
        val tmpSum = left + right + carryOver
        carryOver = tmpSum / 10
        currentNode.next =  ListNode(tmpSum % 10)
        if (canlog) logger.info(s"${tmpSum % 10}")
        if (canlog) logger.info(s"currentNode: $currentNode l1: $currentL1 l2: $currentL2")
        currentNode = currentNode.next
        currentL1 = if(currentL1 != null) currentL1.next else null
        currentL2 = if(currentL1 != null) currentL2.next else null
        Thread.sleep(1000)
      }

      if (canlog) logger.info(s"[ans] $ans")
      ans.next
    }
  }

  val testSuite = Array(
    (ListNode(2, ListNode(4, ListNode(3))),
      ListNode(5, ListNode(6, ListNode(4))),
      ListNode(7,ListNode(0, ListNode(8)))),

  )

  for (e <- testSuite) {
    val ans = Solution.addTwoNumbers(e._1, e._2)
    logger.info(s"${ans} == ${e._3}")
  }
}
