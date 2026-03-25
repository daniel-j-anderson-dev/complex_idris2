module Complex

||| A complex number represented as a pair of `t`s.
public export
data Complex : Type -> Type where
  ||| rectangular `a + bi`
  (:+) : t -> t -> Complex t

public export infixl 6 :+
