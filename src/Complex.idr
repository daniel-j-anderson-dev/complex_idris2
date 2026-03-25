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

public export
normSquared : Num t => Complex t -> t
normSquared (a :+ b) = (a * a) + (b * b)

public export
map : (t -> u) -> Complex t -> Complex u
map f (a :+ b) = f a :+ f b

public export
fromPolar : Num t => Cast t Double => t -> t -> Complex Double
fromPolar r theta = 
  let r'     = cast r
      theta' = cast theta
      a      = r' * cos theta'
      b      = r' * sin theta'
      z      = a :+ b
  in  z
