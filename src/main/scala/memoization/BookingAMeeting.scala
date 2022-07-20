package memoization

import org.apache.log4j.Logger

object BookingAMeeting extends App {

  /**
   * given 2 calenders
   * and a time for a meeting, find all available spots
   * - extract ranges base om ask time
   * - compute availability for both calenders
   * - respond with available slots
   */

  val logger = Logger.getLogger(this.getClass.getName)
  val canlog = true

  case class MyTime(time: String) {
    def hour: Int = time.split(":")(0).toInt

    def minute: Int = time.split(":")(1).toInt

    override def toString: String = {
      val tmpMinute = if (minute < 10) "0" + minute else minute
      val tmpHour = if (hour < 10) "0" + hour else hour
      s"$tmpHour:$tmpMinute"

    }

    def asNum = {
      val tim = time.replace(":", "").toInt
      //if (canlog) logger.info(s"[time] $tim from time $time")
      tim
    }
  }

  def computeRanges(start: MyTime, last: MyTime, targetSlot: MyTime, acc: Set[(MyTime, MyTime)]): Set[(MyTime, MyTime)] = {
    start match {
      case start if start.hour > last.hour => acc
      case start if start == last => acc
      case start =>
        val newRange = extractRange(start, targetSlot)
        val newAcc = acc ++ Set((start, newRange))
        computeRanges(newRange, last, targetSlot, newAcc)
    }
  }

  def extractRange(time: MyTime, target: MyTime): MyTime = {
    val computedMinutes = time.minute + target.minute
    val remainderMinutes = computedMinutes % 60
    val addedHours = computedMinutes / 60
    val newHour = time.hour + addedHours + target.hour
    val newMinutes = if (remainderMinutes < 10) "0" + remainderMinutes else remainderMinutes
    val newRange = MyTime(newHour + ":" + newMinutes)
    //if (canlog) logger.info(s"[new-range] $newRange start: $time to $target")
    newRange
  }

  val testSuite = Array(
    Map("requester" ->
      Map(
        "working-hours" -> IndexedSeq("09:00", "20:00"),
        "booked" -> IndexedSeq(List("09:00", "10:30"), List("12:00", "13:00"), List("16:00", "18:00"))
      ),
      "responder" ->
        Map(
          "working-hours" -> IndexedSeq("10:00", "20:30"),
          "booked" -> IndexedSeq(List("10:00", "11:30"), List("12:30", "14:30"), List("14:30", "15:00"), List("16:00", "17:00"))
        ),
      "target-time" -> "00:30"),
  )


  implicit val myTime = new Ordering[MyTime] {
    override def compare(x: MyTime, y: MyTime): Int =
      x.asNum - y.asNum
  }


  for (e <- testSuite) {
    val requesterAvail = getRanges("requester", e)
    val responderAvail = getRanges("responder", e)
    val availableTimes = requesterAvail.intersect(responderAvail)
    val availableTimesBeauty = mergeAns(availableTimes.toList.sorted)

    logger.info(s"[available-times] ${availableTimes.toList.sorted}")
    logger.info(s"[available-times-beuaty] ${availableTimesBeauty.toList.sorted}")


  }

  def mergeAns(value: List[(BookingAMeeting.MyTime, BookingAMeeting.MyTime)]) = {
    val ans = collection.mutable.ListBuffer.empty[(MyTime, MyTime)]
    if (value != Nil) {
      val last = value.tail.foldLeft(value.head)(
        (a: (MyTime, MyTime), b) =>
          if (a._2 == b._1) (a._1, b._2)
          else {
            ans += a
            b
          }
      )
      ans += last
    }
    else ans
  }

  def getRanges(person: String, e: Map[String, Any]): Set[(MyTime, MyTime)] = {
    val init = MyTime(e(person).asInstanceOf[Map[String, IndexedSeq[String]]]("working-hours").head)
    val last = MyTime(e(person).asInstanceOf[Map[String, IndexedSeq[String]]]("working-hours").last)
    val targetSlot = MyTime(e("target-time").asInstanceOf[String])

    val bookedSlots = for (slot <- e(person).asInstanceOf[Map[String, IndexedSeq[List[String]]]]("booked")) yield {
      val bInit = MyTime(slot.head)
      val bLast = MyTime(slot.last)
      computeRanges(bInit, bLast, targetSlot, Set.empty[(MyTime, MyTime)])
    }

    val workingRangesReq = computeRanges(init, last, targetSlot, Set.empty[(MyTime, MyTime)])
    val bookedReq = bookedSlots.flatten.toSet
    val availabilityReq = workingRangesReq.diff(bookedReq)

    logger.info(s"[working-slots-$person] of $targetSlot are ${workingRangesReq.toList.sorted}")
    logger.info(s"[booked-slots-$person] of $targetSlot are ${bookedReq.toList.sorted}")
    logger.info(s"[requester-availability-$person] of $targetSlot is  ${availabilityReq.toList.sorted}")
    availabilityReq
  }
}
