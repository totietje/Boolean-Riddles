package net.totietje.riddles

object ClaimSolver {
  /**
    * Given that a claim is true, solves it if possible for all variables it contains
    *
    * Returns illegal argument exception either if there are no solutions - claim is false - or multiple solutions.
    * Successfully returns a map of every variable and its value (boolean) if one solution
    */
  def solve(claim: Claim): Map[String, Boolean] = {
    val terms = claim.terms
    val variables = terms.flatten

    // Factorise it - get common terms, eg ABCD + ABC + ABD + AB -> AB(CD + C + D + 1)
    val trueVars = for {
      variable <- variables
      if terms.forall(_.contains(variable))
    } yield variable

    // Remove common terms, eg AB(CD + C + D + 1) -> CD + C + D + 1
    val newTerms = terms.map { term =>
      term.diff(trueVars)
    }

    // The remaining, false, variables
    val newVariables = variables.diff(trueVars)

    // Fully solvable iff all combinations of variables appear as terms,
    // as only then can an expression be written in the form (A + 1)(B + 1)...
    // eg, (C + 1)(D + 1) = CD + C + D + 1, which is every subset of Set(C, D)
    // (where 1 is represented as the empty set)
    if (newVariables.subsets.toSet == newTerms) {
      val falseVars = newVariables
      (trueVars.map((_, true)) ++ falseVars.map((_, false))).toMap
    } else {
      throw new IllegalArgumentException("Unsolvable")
    }
  }

  // Solves multiple claims simultaneously by ANDing them all together first
  def solve(claims: Iterable[Claim]): Map[String, Boolean] = {
    solve(Claim.asOne(claims))
  }
}
