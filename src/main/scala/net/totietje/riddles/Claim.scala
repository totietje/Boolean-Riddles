package net.totietje.riddles

sealed trait Claim {
  /**
    * XOR
    */
  def +(that: Claim): Claim = Claim.Xor(this, that)

  /**
    * AND
    */
  def *(that: Claim): Claim = Claim.And(this, that)

  def simplified: Claim = this
}

object Claim {
  object One extends Claim {
    override def *(that: Claim): Claim = that
  }

  object Zero extends Claim {
    override def *(that: Claim): Claim = Zero

    override def +(that: Claim): Claim = that

    override def toString: String = "0"
  }

  case class Xor(a: Claim, b: Claim) extends Claim {
    override def simplified: Claim = {
      (a.simplified, b.simplified) match {
        case (x, y) if x == y => Zero
        case (Zero, x) => x
        case (x, Zero) => x
        case (Xor(x, y), z) => Xor(x, Xor(y, z)).simplified
        case (term, xor@Xor(_, _)) => xor.without(term) match {
          case Some(thing) => thing
          case None => And(term, xor)
        }
        case (x, y) => And(x, y)
      }
    }

    private def without(term: Claim): Option[Claim] = {
      if (a == term) {
        Some(b)
      } else {
        b match {
          case xor@Xor(_, _) =>xor.without(term).map(Xor(a, _))
          case x if x == term => Some(a)
          case _ => None
        }
      }
    }
  }

  case class And(a: Claim, b: Claim) extends Claim {
    override def simplified: Claim = {
      (a.simplified, b.simplified) match {
        case (x, y) if x == y => x
        case (Xor(x, y), z) => (x * z + y * z).simplified
        case (z, Xor(x, y)) => (x * z + y * z).simplified
        case (x, One) => x
        case (One, x) => x
        case (_, Zero) => Zero
        case (Zero, _) => Zero
        case (And(x, y), z) => And(x, And(y, z)).simplified
        case (x, y@Var(_)) => And(x, y)
        case (variable, and@And(_, _)) => if (and.implies(variable)) and else And(variable, and)
      }
    }

    private def implies(variable: Claim): Boolean = {
      a == variable || (b match {
        case and@And(_, _) => and.implies(variable)
        case b => b == variable
      })
    }
  }

  case class Var(name: String) extends Claim
}