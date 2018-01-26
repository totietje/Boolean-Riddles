package net.totietje.riddles

import net.totietje.riddles.Claim.One

import scala.language.implicitConversions

case class Claim(terms: Set[Set[String]]) extends AnyVal {
  /**
    * XOR
    */
  def +(that: Claim): Claim = {
    Claim(that.terms.diff(this.terms) ++ this.terms.diff(that.terms))
  }

  /**
    * AND
    */
  def *(that: Claim): Claim = Claim(
    that.terms.flatMap { x =>
      this.terms.map((_, x))
    }.toList.map {
      case (x, y) => x ++ y
    }.groupBy(identity).collect {
      case (term, list) if list.size % 2 == 1 => term
    }.toSet
  )

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

  def asOne(iterable: Iterable[Claim]): Claim = {
    iterable.fold(One)(_ * _)
  }

  def not(claim: Claim): Claim = claim.not
}