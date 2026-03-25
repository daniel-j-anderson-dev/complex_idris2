module Complex

||| A complex number represented as a pair of `t`s.
public export
data Complex : Type -> Type where
  ||| rectangular `a + bi`
  (:+) : t -> t -> Complex t

public export infixl 6 :+

public export
add : Num t => Complex t -> Complex t -> Complex t
add (a :+ b) (c :+ d) = (a + c) :+ (b + d)

public export
subtract : Neg t => Complex t -> Complex t -> Complex t
subtract (a :+ b) (c :+ d) = (a - c) :+ (b - d)
