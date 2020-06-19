
module Language.Logic.Number(Number(..), toDouble, number) where

import Data.Ratio

data Number = NumRat Rational
            | NumFloat Double

toDouble :: Number -> Double
toDouble (NumRat r) = fromRational r
toDouble (NumFloat d) = d

alwaysDouble :: (Double -> Double) -> Number -> Number
alwaysDouble f = NumFloat . f . toDouble

number :: (Rational -> a) -> (Double -> a) -> Number -> a
number f _ (NumRat a) = f a
number _ g (NumFloat b) = g b

numberMono :: (Rational -> Rational) -> (Double -> Double) -> Number -> Number
numberMono f g n = number (NumRat . f) (NumFloat . g) n

promoteBin :: (Rational -> Rational -> a) -> (Double -> Double -> a) -> Number -> Number -> a
promoteBin f _ (NumRat a) (NumRat b) = f a b
promoteBin _ g a b = g (toDouble a) (toDouble b)

promoteBinMono :: (Rational -> Rational -> Rational) -> (Double -> Double -> Double) -> Number -> Number -> Number
promoteBinMono f _ (NumRat a) (NumRat b) = NumRat $ f a b
promoteBinMono _ g a b = NumFloat $ g (toDouble a) (toDouble b)

-- Since floating-point NaN doesn't normally satisfy any semblance of
-- an equivalence or ordering relation, we'll use a shim to fix that
-- here, so we can safely use these things in data structures and
-- whatnot.

eqDouble :: Double -> Double -> Bool
eqDouble a b | isNaN a && isNaN b = True
eqDouble a b = a == b

ordDouble :: Double -> Double -> Ordering
ordDouble a b | isNaN a && isNaN b = EQ
ordDouble a _ | isNaN a            = GT
ordDouble _ b |            isNaN b = LT
ordDouble a b = a `compare` b

instance Eq Number where
    (==) = promoteBin (==) eqDouble

instance Ord Number where
    compare = promoteBin compare ordDouble

instance Show Number where
    showsPrec _ (NumRat r)
        | denominator r == 1 = shows (numerator r)
        | otherwise = shows (numerator r) . ("r" ++) . shows (denominator r)
    showsPrec _ (NumFloat d) = shows d

instance Num Number where
    (+) = promoteBinMono (+) (+)
    negate = numberMono negate negate
    (*) = promoteBinMono (*) (*)
    abs = numberMono abs abs
    signum = numberMono signum signum
    fromInteger = NumRat . fromInteger

instance Fractional Number where
    recip (NumRat x)
        | x == 0 = NumFloat (recip 0)
        | otherwise = NumRat (recip x)
    recip (NumFloat x) = NumFloat (recip x)
    fromRational = NumRat

instance Real Number where
    toRational (NumRat r) = r
    toRational (NumFloat f) = toRational f

instance RealFrac Number where
    properFraction (NumRat r) =
        let (i, r') = properFraction r in
        (i, NumRat r')
    properFraction (NumFloat d) =
        let (i, d') = properFraction d in
        (i, NumFloat d')

instance Floating Number where
    pi = NumFloat pi
    exp = alwaysDouble exp
    log = alwaysDouble log
    sin = alwaysDouble sin
    cos = alwaysDouble cos
    asin = alwaysDouble asin
    acos = alwaysDouble acos
    atan = alwaysDouble atan
    sinh = alwaysDouble sinh
    cosh = alwaysDouble cosh
    asinh = alwaysDouble asinh
    acosh = alwaysDouble acosh
    atanh = alwaysDouble atanh
