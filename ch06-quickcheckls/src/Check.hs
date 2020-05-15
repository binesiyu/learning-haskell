{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE GADTs         #-}

module Check
where

import Data.Bits
import Data.List
import Data.Char
import Data.Monoid
import System.Random
import Control.Monad (join)
import Control.Arrow ((&&&))
import Text.Show.Functions
import qualified Data.Map as M

newtype Distribution a = Dist { runDist :: Int -> StdGen -> a } deriving (Functor)

instance Applicative Distribution where
  pure a = Dist $ (\_ _ -> a)
  Dist f <*> Dist a = Dist $ \s r -> let (r1, r2) = split r in f s r1 (a s r2)

frequency :: [(Int, Distribution a)] -> Distribution a
frequency fs = do { as <- sequenceA (fmap snd fs); w <- choose (0, head ws - 1); pure (pick (tail ws) as w) }
  where
    ws = scanr1 (+) (fmap fst fs)
    pick []     (a:as) r = a
    pick (w:ws) (a:as) r = if r >= w then a else pick ws as r
data Rose a = Rose a [Rose a] deriving (Show)

instance Arbitrary a => Arbitrary (Rose a) where
  arbitrary = sized arbRose where
    arbRose 0 = Rose <$> arbitrary <*> pure []
    arbRose n = frequency
      [ (1, arbRose 0)
      , (2, Rose <$> arbitrary <*> resize (n `div` 2) arbitrary)
      ]

sized :: (Int -> Distribution a) -> Distribution a
sized f = Dist $ \s -> runDist (f s) s

resize :: Int -> Distribution a -> Distribution a
resize s d = Dist $ \_ -> runDist d s

scale :: (Int -> Int) -> Distribution a -> Distribution a
scale f g = sized $ \s -> resize (f s) g

class Arbitrary a where
  arbitrary :: Distribution a
  shrink :: a -> Succs a
  shrink = flip Succs []

choose :: Random a => (a, a) -> Distribution a
choose r = Dist $ const (fst . randomR r)

chooseAny :: Random a => Distribution a
chooseAny = Dist $ const (fst . random)

elements :: [a] -> Distribution a
elements as = do { n <- choose (0, length as - 1); pure (as !! n) }

oneOf :: [Distribution a] -> Distribution a
oneOf gs = do { as <- sequenceA gs; n <- choose (0, length gs - 1); pure (as !! n) }

vectorOf :: Distribution a -> Int -> Distribution [a]
vectorOf g = sequenceA . flip replicate g

listOf :: Distribution a -> Distribution [a]
listOf g = do { as <- sized (vectorOf g); n <- sized (\s -> choose (0, s)); pure (take n as) }

listOf1 :: Distribution a -> Distribution [a]
listOf1 g = do { as <- sized (vectorOf g); n <- sized (\s -> choose (1, s)); pure (take n as) }

orderedList :: (Ord a, Arbitrary a) => Distribution [a]
orderedList = sort <$> listOf arbitrary

promote :: (a -> Distribution b) -> Distribution (a -> b)
promote f = Dist $ \s r a -> runDist (f a) s r

variant :: Int -> Distribution a -> Distribution a
variant n d = Dist $ \s r -> runDist d s (iter r)
  where
    cnt  = maybe 64 id (bitSizeMaybe n)
    bits = testBit n <$> [0..(cnt-1)]
    iter = flip (foldr (\b r -> if b then fst (split r) else snd (split r))) bits

class Coarbitrary a where
  coarbitrary :: a -> Distribution b -> Distribution b

instance (Arbitrary a, Coarbitrary b) => Arbitrary (b -> a) where
  arbitrary = promote (flip coarbitrary arbitrary)

data Result = Result
  { ok        :: Maybe Bool
  , labels    :: M.Map String Int
  , classes   :: M.Map String Int
  , arguments :: [String]
  , seed      :: Int
  , numTests  :: Int
  } deriving (Show)

instance Semigroup Result where
  r@(Result (Just False) _ _ _ _ _) <> _           = r
  _ <> r@(Result (Just False) _ _ _ _ _)           = r
  Result Nothing _ _ _ _ _ <> r                    = r
  r <> Result Nothing _ _ _ _ _                    = r
  Result _ ll lc la _ ln <> Result _ rl rc ra _ rn = Result (Just True) (M.unionWith (+) ll rl) (M.unionWith (+) lc rc) (la ++ ra) 0 (ln + rn)

instance Monoid Result where
  mempty = Result Nothing M.empty M.empty [] 0 0

data Succs a = Succs { current :: a, successor :: [a] } deriving (Show, Functor)

instance Applicative Succs where
  pure = flip Succs []
  Succs f fs <*> Succs a as = Succs (f a) ((($ a) <$> fs) ++ (f <$> as))

instance Monad Succs where
  return = flip Succs []
  Succs a as >>= mf = Succs (current (mf a)) (fmap (current . mf) as ++ successor (mf a))

newtype Property = Prop { runProperty :: FreeA Distribution (Succs Result) }

success = (Prop . pure . pure) $ mempty { ok = Just True , numTests = 1 }
failure = (Prop . pure . pure) $ mempty { ok = Just False, numTests = 1 }
discard = (Prop . pure . pure) $ mempty { ok = Nothing   , numTests = 1 }

class Testable a where
  property :: a -> Property

instance Testable Bool where
  property b = if b then success else failure

forAll' :: Testable b => (a -> Succs a) -> (a -> String) -> Distribution a -> (a -> b) -> Property
forAll' shrinker printer da f = Prop $ do
  sa <- lift (fmap shrinker da)
  sg <- lift (pure <$> promote (unlift . runProperty . property))
  pure $ do
    g <- sg
    (b, arg) <- fmap (f &&& printer) sa
    logArg arg <$> g b

forAll :: (Arbitrary a, Show a, Testable b) => Distribution a -> (a -> b) -> Property
forAll = forAll' shrink show

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll' shrink show arbitrary f

sample :: Show a => Distribution a -> [a]
sample g = runDist (sequenceA [resize n g | n <- [0..9]]) 10 (mkStdGen 10)

quickCheck :: Testable a => a -> Int -> Int -> Result
quickCheck a s r = foldMap id . take 100 $ zipWith3 (\a b c -> small (execProperty a b c)) (repeat (property a)) sizes rands
  where
    sizes = (+1) <$> [s..]
    rands = fst  <$> iterate (split . snd) (split (mkStdGen r))
    small (Succs x xs) = if ok x /= Just False then x else maybe x id (find ((== Just False) . ok) xs)

prop_rev :: Eq a => [a] -> [a] -> Bool
prop_rev = \xs ys -> reverse (xs ++ ys) == reverse xs ++ reverse ys

prop_gcd :: Property
prop_gcd = forAll (choose (0,60)) $ \a -> forAll (choose (0,60)) ((\b -> a > 1 && b > 1 ==> gcd a b /= 3) :: Int -> Property)

prop_fuse :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
prop_fuse = \f g as -> fmap g (fmap f as) == fmap (f . g) as

instance Testable Property where
  property = id

logSeed  s r = r { seed = s }
logArg   a (Result r l c as s n) = Result r l c (maybe as (const (a:as)) r) s n
logLabel l (Result r ls c a s n) = Result r (M.insertWith (+) l 1 ls) c a s n

infixr 0 ==>
(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> _ = discard

label :: Testable a => String -> a -> Property
label s a = Prop $ fmap (logLabel s) <$> (runProperty (property a))

classify :: Testable a => Bool -> String -> a -> Property
classify True = label
classify False = const property

collect :: (Show a, Testable b) => a -> b -> Property
collect = label . show

execProperty :: Property -> Int -> StdGen -> Succs Result
execProperty p = go (runProperty p)
  where
    go :: FreeA Distribution a -> Int -> StdGen -> a
    go (NilA a)    _ _ = a
    go (ConsA d f) s r = let (r1, r2) = split r in (go f s r1) (runDist d s r2)

instance Arbitrary () where
  arbitrary = pure ()

instance Arbitrary Bool where
  arbitrary = chooseAny
  shrink b = Succs b (if b then [False] else [])

instance Arbitrary Char where
  arbitrary = choose ('\0', '\127')
  shrink c = Succs c . nub . filter (`isLT` c) $ (toLower c : smalls)
    where
      smalls = "abc123 ABC\n?"
      isLT a b = measure a < measure b
      measure c = (not (isLower c), not (isDigit c), (c /= ' '), not (isUpper c), not (isSpace c), not (c `elem` "\n?."), c)

instance Arbitrary Int where
  arbitrary = sized $ \n -> choose (-n, n)
  shrink n = Succs n ([abs n | n < 0] ++ halves) where
    halves = takeWhile (\x -> abs x < abs n) (0:[ n - i | i <- tail (iterate (`quot` 2) n) ])

instance Arbitrary a => Arbitrary [a] where
  arbitrary = listOf arbitrary
  shrink [] = Succs [] []
  shrink as = Succs as ((flip take as <$> ns) ++ shrinkOne as)
    where
      ns = successor (shrink (length as))
      shrinkOne [] = []
      shrinkOne (x:xs) = [(x':xs) | x' <- successor (shrink x)] ++ [(x:xs') | xs' <- shrinkOne xs]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = do { a <- arbitrary; b <- arbitrary; pure (a,b) }
  shrink (a, b) = do { a' <- shrink a; b' <- shrink b; pure (a', b') }

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = oneOf [Left <$> arbitrary, Right <$> arbitrary]
  shrink (Left a)  = Left  <$> shrink a
  shrink (Right a) = Right <$> shrink a

instance Coarbitrary () where
  coarbitrary a = id

instance Coarbitrary Bool where
  coarbitrary a = if a then variant 0 else variant 1

instance Coarbitrary Int where
  coarbitrary = variant

instance Coarbitrary Char where
  coarbitrary = variant . ord

instance (Coarbitrary a, Coarbitrary b) => Coarbitrary (a, b) where
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b) where
  coarbitrary (Left a) = variant 0 . coarbitrary a
  coarbitrary (Right b) = variant 1 . coarbitrary b

instance Coarbitrary a => Coarbitrary [a] where
  coarbitrary [] = variant 0
  coarbitrary (a:as) = variant 1 . coarbitrary a . coarbitrary as

data FreeA f a where
  NilA  :: a -> FreeA f a
  ConsA :: f x -> FreeA f (x -> a) -> FreeA f a

instance Functor (FreeA f) where
  fmap f (NilA a)    = NilA (f a)
  fmap f (ConsA x g) = ConsA x ((f.) <$> g)

instance Applicative (FreeA f) where
  pure = NilA
  (NilA f)     <*> ga = f <$> ga
  (ConsA x gf) <*> ga = ConsA x $ do { f <- gf; a <- ga; pure (flip f a) }

type NatF f g = forall a. f a -> g a -- f, g must be Functor
type NatA f g = forall a. f a -> g a -- f, g must be Applicative

hoist :: (Functor f, Applicative g) => NatF f g -> NatA (FreeA f) g
hoist t (NilA a)    = pure a
hoist t (ConsA x g) = hoist t g <*> t x

lower :: (Functor f, Applicative g) => NatA (FreeA f) g -> NatF f g
lower t = t . flip ConsA (pure id)

lift :: Functor f => f a -> FreeA f a
lift = lower id

unlift :: Applicative f => FreeA f a -> f a
unlift = hoist id
