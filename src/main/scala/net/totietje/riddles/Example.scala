package net.totietje.riddles

import Claim._

object Example {
  val claims: Seq[Claim] = Seq(
    "A" says ("F" wouldSay "A".isLying),
    "B" says ("I" xor "M"),
    "C" says (("D" and "L" and "M") or not ("D" or "L" or "M")),
    "D" says ("B" sameAs "Y"),
    "E" says ("E" sameAs "T"),
    "F" says ("Q" and "X"),
    "G" says ("G" differentFrom "Z"),
    "H" says "G".isLying,
    "I" says "M".isTruthful,
    "J" says ("B" wouldSay "F".isTruthful),
    "K" says "L".isTruthful,
    "L" says "R".isLying,
    "M" says "E".isLying,
    "N" says ("W".isLying or "Z".isTruthful),
    "O" says ("A".isLying and "Q".isLying),
    "P" says ("D" sameAs "H"),
    "Q" says ("O" wouldSay "Q".isTruthful),
    "R" says ("X".isTruthful and "B".isTruthful),
    "S" says "L".isLying,
    "T" says ("C" wouldSay "T".isTruthful),
    "U" says ("I" wouldSay "R".isLying),
    "V" says ("R" wouldSay "U".isTruthful),
    "W" says ("I".isTruthful or "N".isLying),
    "X" says ("R" wouldSay "S".isTruthful),
    "Y" says ("K".isLying and "S".isLying),
    "Z" says "H".isLying,
  )

  def main(args: Array[String]): Unit = {
    // Takes about 2 seconds
    println(ClaimSolver.solve(claims))
  }
}
