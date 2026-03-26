module Complex

||| A complex number represented as a pair of `t`s.
public export
data Complex : Type -> Type where
  ||| rectangular `a + bi`
  (:+) : t -> t -> Complex t

public export infixl 6 :+

||| Add two complex numbers. in rectangular coordinates `(a + bi) + (c + di) = (a + c) + (b + d)i`
public export
add : Num t => Complex t -> Complex t -> Complex t
add (a :+ b) (c :+ d) = (a + c) :+ (b + d)

||| Subtract two complex numbers.
||| in rectangular coordinates `(a + bi) - (c + di) = (a - c) + (b - d)i`
public export
subtract : Neg t => Complex t -> Complex t -> Complex t
subtract (a :+ b) (c :+ d) = (a - c) :+ (b - d)

||| Calculates the norm squared
||| in rectangular coordinates `|a + bi|^2 = a^2 + b^2`
public export
normSquared : Num t => Complex t -> t
normSquared (a :+ b) = (a * a) + (b * b)

||| Apply `f` to the real component and `g` to the and imaginary component
||| in rectangular coordinates `mapUniform(f, (a + bi)) = f(a) + f(b)i`
public export
map : (t -> u) -> (t -> u) -> Complex t -> Complex u
map f g (a :+ b) = f a :+ g b

||| Apply `f` to both the real and imaginary component
||| in rectangular coordinates `map(f, g, (a + bi)) = f(a) + g(b)i`
public export
mapUniform : (t -> u) -> Complex t -> Complex u
mapUniform f = map f f

||| Multiply a `scalar` and  a complex number `z`
||| in rectangular coordinates `s * (a + b * i) = (s * a) + (s * b * i)`
public export
scalarMultiply : Num t => t -> Complex t -> Complex t
scalarMultiply scalar z = mapUniform (scalar *) z

||| The conjugate of a complex number; that is `a + (b * i) = `
public export
conjugate : Neg t => Complex t -> Complex t
conjugate (a :+ b) = a :+ (- b)

||| Create a `Complex Double` from a radius and angle.
||| `r∠θ = r (cos(theta) + i * sin(theta))`
public export
fromPolar : Cast t Double => t -> t -> Complex Double
fromPolar r theta = 
  let r'     = cast r
      theta' = cast theta
      a      = r' * cos theta'
      b      = r' * sin theta'
      z      = a :+ b
  in  z

public export
real : Complex t -> t
real (a :+ _) = a

public export
imaginary : Complex t -> t
imaginary (_ :+ b) = b

public export
elementWiseMultiply : Num t => Complex t -> Complex t -> Complex t
elementWiseMultiply (a :+ b) (c :+ d) = a * c :+ b * d

||| `elementWiseDivide numerator denominator`
public export
elementWiseDivide : Fractional t => Complex t -> Complex t -> Complex t
elementWiseDivide (a :+ b) (c :+ d) = a / c :+ b / d

