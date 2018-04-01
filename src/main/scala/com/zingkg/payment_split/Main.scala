package com.zingkg.payment_split

import com.github.tototoshi.csv.CSVReader

object Main extends App {
  val reader = CSVReader.open(args(0))
  val allLines = reader.all().drop(1)
  reader.close()

  val paymentMaps = allLines.map(Split.parseLine)
  val accumulatedAmounts = Split.accumulatedAmountOwed(paymentMaps)
  val simplifiedDifferences = Split.simplifyDeltas(accumulatedAmounts)
  println("from,to,amount")
  simplifiedDifferences.foreach { case ((from, to), payment) =>
    val dollars = payment / 100
    val cents = payment % 100
    println(s"$from,$to,$dollars.$cents")
  }
}

object Split {
  def parseLine(line: Seq[String]): Map[(String, String), Long] = {
    val paidBy = line.head.trim.toLowerCase
    val amount = parseAmount(line(1))
    val appliesTo = line(2).replaceAll("\"", "").split(",").map(_.trim.toLowerCase).distinct.toList
    val splitAmount = amount / appliesTo.length
    appliesTo.filter(_ != paidBy).map { from =>
      ((from, paidBy), splitAmount)
    }.toMap
  }

  private def parseAmount(amount: String): Long = {
    val decimalIndex = amount.indexOf('.')
    if (decimalIndex == -1) {
      amount.toLong * 100
    } else {
      val cents = amount.substring(decimalIndex + 1)
      val dollars = amount.substring(0, decimalIndex)
      dollars.toLong * 100 + cents.toLong
    }
  }

  def accumulatedAmountOwed(
    owedAmounts: Seq[Map[(String, String), Long]]
  ): Map[(String, String), Long] =
    owedAmounts.foldLeft(Map.empty[(String, String), Long]) { case (owed, accumulated) =>
      val updatedMap = owed.map { case (fromTo, amount) =>
        (fromTo, accumulated.get(fromTo).map(_ + amount).getOrElse(amount))
      }
      accumulated ++ updatedMap
    }

  def simplifyDeltas(amountOwed: Map[(String, String), Long]): Map[(String, String), Long] = {
    val keys = amountOwed.keySet.map(tuple => Set(tuple._1, tuple._2))
    keys.map { set =>
      val forwardKey = (set.head, set.last)
      val forward = amountOwed.getOrElse(forwardKey, 0L)
      val reverseKey = (set.last, set.head)
      val reverse = amountOwed.getOrElse(reverseKey, 0L)
      if (forward > reverse)
        (forwardKey, forward - reverse)
      else
        (reverseKey, reverse - forward)
    }.toMap
  }
}
