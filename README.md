# Boolean-Riddles

Inspired by [this](http://www.maths.manchester.ac.uk/mathsbombe/problem.php?index=2) problem, which was slow and
boring to do by hand, this takes a list of statements, written in near-English, and figures out who's lying.

This works when each person is either always lies or always tells the truth.

For example, take the following:

```scala
import Claim._

val claims: Seq[Claim] = Seq(
  "A" says ("C" wouldSay "A".isLying),
  "B" says ("A".isTruthful),
  "C" says "B".isLying,
  "D" says "C".isTruthful
)

println(ClaimSolver.solve(claims))
```

This first converts each claim into boolean algebra (Where + is XOR and * is AND), converting the claims into:

    1 + A + (1 + C + (1 + A)) = 1 + C
    1 + B + A
    1 + C + (1 + B) = C + B
    1 + D + C

It then ANDs all claims together, producing:

    (1 + C)(1 + B + A)(C + B)(1 + D + C) = ABCD + ABC + ABD + AB

Finally, it factorises the expression:

    AB(C + 1)(D + 1)

Yielding the solution A = 1, B = 1, C = 0, D = 0 - so A and B are truthful while C and D are liars.