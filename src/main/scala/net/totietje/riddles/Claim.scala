package net.totietje.riddles

import net.totietje.riddles.Claim.One

import scala.language.implicitConversions

/**
  * Represents a claim as boolean algebra, the maths version, not the electronics version - so + is XOR and * is AND.
  *
  * This is implemented via a set of XOR'd terms, where each term is a set of AND'd variables.
  *
  * eg, Claim(Set(Set("A", "B"), Set("A", "C"))) represents AB + AC
  */
case class Claim(terms: Set[Set[String]]) extends AnyVal {
  /**
    * XOR
    */
  def +(that: Claim): Claim = {
    // Cancel like terms - keep terms that are in this but not that, or that but not this
    Claim(that.terms.diff(this.terms) ++ this.terms.diff(that.terms))
  }

  /**
    * AND
    */
  def *(that: Claim): Claim = Claim(
    // Get all pairs of terms, 1st from this, 2nd from that
    // eg (A + B + C)(D + E + F) -> [(A, D), (A, E), (A, F), (B, D), (B, E), (B, F), (C, D), (C, E), (C, F)]
    that.terms.flatMap { x =>
      this.terms.map((_, x))
    }.toList.map {
      // And terms together, so that (ABC, ABD) -> ABCD
      case (x, y) => x ++ y
    }.groupBy(identity).collect {
      // Cancel like terms, as x + x = 0 - eg [ABC, ABC, ABD] -> [ABD]
      // Only keep term if odd number of them
      case (term, list) if list.size % 2 == 1 => term
    }.toSet
  )

  // Subtraction and addition are equivalent in boolean algebra
  def -(that: Claim): Claim = this + that

  def and(that: Claim): Claim = this * that

  def or(that: Claim): Claim = Claim.One + (Claim.One + this) * (Claim.One + that)

  def xnor(that: Claim): Claim = (this + that).not

  def xor(that: Claim): Claim = this + that

  def not: Claim = Claim.One + this

  def simplified: Claim = this

  def says(claim: Claim): Claim = One + this + claim

  def wouldSay(claim: Claim): Claim = One + this + claim

  def isLying: Claim = not

  def isTruthful: Claim = this

  def sameAs(claim: Claim): Claim = One + this + claim

  def differentFrom(claim: Claim): Claim = this + claim

  override def toString: String = {
    if (terms.isEmpty) {
      return "0"
    }
    terms.map {
      case one if one.isEmpty => "1"
      case vars => vars.mkString
    }.mkString(" + ")
  }
}

object Claim {
  val One = Claim(Set(Set()))
  val Zero = Claim(Set())

  implicit def toVar(name: String): Claim = Claim(Set(Set(name)))

  /**
    * Combines many claims into one equivalent one by multiplying them all together
    */
  def asOne(iterable: Iterable[Claim]): Claim = {
    iterable.fold(One)(_ * _)
  }

  def not(claim: Claim): Claim = claim.not
}