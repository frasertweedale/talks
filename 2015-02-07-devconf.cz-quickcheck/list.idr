module List

%default total

rev : List a -> List a
rev [] = []
rev (x :: xs) = rev xs ++ [x]

revUnit : rev [a] = [a]
revUnit = Refl

revApp : (xs, ys : List a)
      -> rev (xs ++ ys) = rev ys ++ rev xs
revApp = ?proof_revApp

---------- Proofs ----------

List.proof_revApp = proof
  intros
  induction xs
  compute
  rewrite (appendNilRightNeutral (rev ys))
  trivial
  intros
  compute
  rewrite sym ihl__0
  rewrite (appendAssociative (rev ys) (rev l__0) [t__0])
  trivial
