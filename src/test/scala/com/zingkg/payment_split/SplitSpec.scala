package com.zingkg.payment_split

import org.scalatest.{ MustMatchers, WordSpec }

class SplitSpec extends WordSpec with MustMatchers {
  "Split#parseLine" should {
    "parse a line correctly" in {
      val actual = Split.parseLine(Seq("bob", "20.11" , "\"bob,jerry\""))
      val expected = Map(
        ("jerry", "bob") -> 1005
      )
      actual mustBe expected
    }
  }

  "Split#amountOwed" should {
    "accumulate the amount correctly" in {
      val actual = Split.accumulatedAmountOwed(
        Seq(Map(("a", "b") -> 1000), Map(("a", "b") -> 2000, ("c", "b") -> 1000))
      )
      val expected = Map(("a", "b") -> 3000, ("c", "b") -> 1000)
      actual mustBe expected
    }
  }

  "Split.simplifyDeltas" should {
    "simplify the difference correctly" in {
      val actual = Split.simplifyDeltas(Map(("a", "b") -> 2000, ("b", "a") -> 3000))
      val expected = Map(("b", "a") -> 1000)
      actual mustBe expected
    }
  }
}
