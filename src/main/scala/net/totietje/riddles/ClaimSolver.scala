package net.totietje.riddles

object ClaimSolver {
  /**
    * Given that a claim is true, solves
    */
  def solve(claim: Claim): Map[String, Boolean] = {
    val terms = claim.terms
    val variables = terms.flatten

    val trueVars = for {
      variable <- variables
      if terms.forall(_.contains(variable))
    } yield variable

    val newTerms = terms.map { term =>
      // Remove all true variables from terms
      term.diff(trueVars)
    }

    val newVariables = variables.diff(trueVars)

    if (newVariables.subsets.toSet == newTerms) {
      val falseVars = newVariables
      (trueVars.map((_, true)) ++ falseVars.map((_, false))).toMap
    } else {
      throw new IllegalArgumentException("Unsolvable")
    }
  }

  def solve(claims: Iterable[Claim]): Map[String, Boolean] = {
    solve(Claim.asOne(claims))
  }
}
