package net.totietje.riddles

import net.totietje.riddles.Claim.{One, Zero}
import net.totietje.riddles.Tests.arbitraryClaim
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.Matchers._

class Tests extends FunSuite with PropertyChecks{
  test("A claim XOR itself should be 0") {
    forAll { claim: Claim =>
      assert(claim + claim == Zero)
    }
  }

  test("A claim AND itself should not change") {
    forAll { claim: Claim =>
      assert(claim * claim == claim)
    }
  }

  test("A claim OR itself should not change") {
    forAll { claim: Claim =>
      assert((claim or claim) == claim)
    }
  }

  test("A claim XOR its opposite should be 1") {
    forAll { claim: Claim =>
      assert((claim + claim.not) == One)
    }
  }

  test("A claim AND its opposite should be 0") {
    forAll { claim: Claim =>
      assert((claim * claim.not) == Zero)
    }
  }

  test("A claim OR its opposite should be 1") {
    forAll { claim: Claim =>
      assert((claim or claim.not) == One)
    }
  }

  test("A claim XOR 0 should not change") {
    forAll { claim: Claim =>
      assert(claim + Zero == claim)
    }
  }

  test("A claim AND 0 should be 0") {
    forAll { claim: Claim =>
      assert(claim * Zero == Zero)
    }
  }

  test("A claim OR 0 should not change") {
    forAll { claim: Claim =>
      assert((claim or Zero) == claim)
    }
  }

  test("A claim XOR 1 should be the opposite") {
    forAll { claim: Claim =>
      assert(claim + One == claim.not)
    }
  }

  test("A claim AND 1 should not change") {
    forAll { claim: Claim =>
      assert(claim * One == claim)
    }
  }

  test("A claim OR 1 should be 1") {
    forAll { claim: Claim =>
      assert((claim or One) == One)
    }
  }

  test("XOR is associative") {
    forAll { (a: Claim, b: Claim) =>
      assert(a + b == b + a)
    }
  }

  test("AND is associative") {
    forAll { (a: Claim, b: Claim) =>
      assert(a * b == b * a)
    }
  }

  test("OR is associative") {
    forAll { (a: Claim, b: Claim) =>
      assert((a or b) == (b or a))
    }
  }

  test("A claim inverted twice should be the original claim") {
    forAll { claim: Claim =>
      assert(claim.not.not == claim)
    }
  }

  test("Testing a claim without necessary variables should throw exception") {
    forAll(Tests.variablesNotProvided) { (claim: Claim, map: Map[String, Boolean]) =>
      assertThrows[IllegalArgumentException] {
        claim.test(map)
      }
    }
  }

  test("Testing ANDs should not necessarily need all variables") {
    forAll(Tests.variablesPartiallyProvided) { (claim: Claim, map: Map[String, Boolean]) =>
      noException should be thrownBy {
        claim.test(map)
      }
    }
  }

  test("Testing 1 should always return true") {
    forAll { map: Map[String, Boolean] =>
      assert(One.test(map))
    }
  }

  test("Testing 0 should always return false") {
    forAll { map: Map[String, Boolean] =>
      assert(!Zero.test(map))
    }
  }

  test("Solvable claims") {
    forAll(Tests.solvableClaims) { (claim: Claim, map: Map[String, Boolean]) =>
      assert(ClaimSolver.solve(claim) == map)
      assert(claim.test(map))
    }
  }

  test("Unsolvable claims") {
    forAll(Tests.unsolvableClaims) { (claim: Claim) =>
      assertThrows[IllegalArgumentException] {
        ClaimSolver.solve(claim)
      }
    }
  }
}

object Tests {
  implicit val arbitraryClaim: Arbitrary[Claim] = Arbitrary(
    for {
      terms <- Arbitrary.arbitrary[Set[Set[String]]]
    } yield Claim(terms)
  )

  val solvableClaims = Table(
    ("claim", "solution"),
    (One, Map[String, Boolean]()),
    (Claim(Set(Set("A"))), Map("A" -> true)),
    (One + "A", Map("A" -> false)),
    (Claim(Set(Set("A", "B", "C", "D"), Set("A", "B", "C"), Set("A", "B", "D"), Set("A", "B"))),
      Map("A" -> true, "B" -> true, "C" -> false, "D" -> false)),
  )

  val unsolvableClaims = Table(
    "claim",
    Zero,
    Claim(Set(Set("A"), Set("B"))),
    Claim(Set(Set("A"), Set("B"), Set())),
    Claim(Set(Set("A"), Set("B", "A"), Set())),
    Claim(Set(Set("A", "C"), Set("B", "A"), Set())),
  )

  val variablesNotProvided = Table(
    ("claim", "test"),
    (Claim(Set(Set("A"))), Map[String, Boolean]()),
    (Claim(Set(Set("A"), Set("B"))), Map("A" -> true)),
    (Claim(Set(Set("A", "B"))), Map("C" -> true)),
  )

  val variablesPartiallyProvided = Table(
    ("claim", "test"),
    (Claim(Set(Set("A", "B"))), Map("A" -> false)),
    (Claim(Set(Set("A", "B"))), Map("B" -> false)),
    (Claim(Set(Set("A", "B"), Set("A"))), Map("A" -> false)),
    (Claim(Set(Set("A", "B"), Set())), Map("B" -> false)),
  )
}