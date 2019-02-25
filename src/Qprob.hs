{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language UndecidableInstances #-}

module Qprob where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Linear.Epsilon (nearZero)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Complex as Complex

data Setting = Classical | Quantum

--type family Setting' s = (s' :: Type) | s' -> s

class V (s :: Setting) where
  type Setting' s = (s' :: Type) | s' -> s
  oneS :: Setting' s
  plusS :: Setting' s -> Setting' s -> Setting' s
  zeroS :: Setting' s
  timesS :: Setting' s -> Setting' s -> Setting' s
  negateS :: Setting' s -> Setting' s
  nearZeroS :: Setting' s -> Bool

instance V 'Classical where
  type Setting' 'Classical = Double
  oneS = one; plusS = plus; zeroS = zero; timesS = times; negateS = negate; nearZeroS = nearZero;
  {-# inline oneS #-}
  {-# inline plusS #-}
  {-# inline zeroS #-}
  {-# inline timesS #-}
  {-# inline negateS #-}
  {-# inline nearZeroS #-}

instance V 'Quantum where
  type Setting' 'Quantum = Complex Double
  oneS = one; plusS = plus; zeroS = zero; timesS = times; negateS = negate; nearZeroS = nearZero;
  {-# inline oneS #-}
  {-# inline plusS #-}
  {-# inline zeroS #-}
  {-# inline timesS #-}
  {-# inline negateS #-}
  {-# inline nearZeroS #-}

data Space (s :: Setting) a = Space !a !(Setting' s)

deriving instance (Show a, Show (Setting' s)) => Show (Space s a)
deriving instance (Eq a, Eq (Setting' s)) => Eq (Space s a)

mapSpace :: (Setting' s -> Setting' s) -> Space s a -> Space s a
mapSpace f (Space a s) = Space a (f s)

-- | @W s a@ is a vector space whose basis elements are labelled
--   by objects of type @a@ and where the coefficients are of type @Settings'' s@.
--
--   This is very similar to standard probability monads except that we
--   allow probabilities to be types other than 'Double'.
newtype W (s :: Setting) a = W { runW :: [Space s a] }

deriving instance (Show a, Show (Setting' s)) => Show (W s a)
deriving instance (Eq a, Eq (Setting' s)) => Eq (W s a)

-- | Transform the probabilities inside of 'W'.
mapW :: (Setting' s -> Setting' s) -> W s a -> W s a
mapW f (W l) = W (List.map (mapSpace f) l)

instance Semigroup (W s a) where
  W x <> W y = W (x <> y)
  {-# inline (<>) #-}

instance Monoid (W s a) where
  mempty = W mempty
  {-# inline mempty #-}

instance Functor (W s) where
  fmap f w = fmapW f w
  {-# inline fmap #-}

fmapW :: (a -> b) -> W s a -> W s b
fmapW f (W l) = W (List.map (\(Space a p) -> Space (f a) p) l)
{-# NOINLINE [1] fmapW #-}

{-# RULES "fmapW/coerce" fmapW coerce = coerce #-}

instance V s => Applicative (W s) where
  pure x = W [Space x oneS]
  {-# inline pure #-}
  W fs <*> W xs = W
    ( do Space f a <- fs
         Space x b <- xs
         pure (Space (f x) (timesS a b))
    )

infixl 7 .*
(.*) :: V s => Setting' s -> W s a -> W s a
a .* b = mapW (a `timesS`) b

type P a = W 'Classical a
type Q a = W 'Quantum   a

star :: Q a -> Q a
star = mapW Complex.conjugate

kron :: V s => W s a -> W s c -> W s (a,c)
kron (W x) (W y) = W
  ( do Space a r1 <- x
       Space c r2 <- y
       pure (Space (a,c) (r1 `timesS` r2))
  )

collect :: (V s, Ord a) => W s a -> W s a
collect (W l) = W $ toList . fromListWith plusS $ l
  where
    toList = Map.foldrWithKey (\k x xs -> (Space k x):xs) []
    fromListWith f xs = fromListWithKey (\_ x y -> f x y) xs
    fromListWithKey f xs =
      let ins t (Space a s) = Map.insertWithKey f a s t
      in List.foldl' ins mempty xs

instance V s => Monad (W s) where
  l >>= f = W $ List.concatMap (\(Space (W d) p) -> List.map (\(Space x q) -> (Space x (p `timesS` q))) d) (runW $ fmap f l)

-- | When we come to observe the state of a quantum system, the quantum state becomes an ordinary probablistic one.
observe :: Ord a => Q a -> P a
observe = W . List.map (\(Space a w) -> Space a (Complex.magnitude (w `times` w))) . runW . collect

rotate :: Double -> Bool -> Q Bool
rotate theta = \case
  True ->
    let theta' = theta :+ 0
    in subtract (cos (theta' / 2) .* pure True) (sin (theta' / 2) .* pure False)
  False ->
    let theta' = theta :+ 0
    in (cos (theta' / 2) .* pure False) <> (sin (theta' / 2) .* pure True)

repeat :: Int -> (a -> a) -> (a -> a)
repeat 0 _ = id
repeat n f = repeat (n - 1) f . f

repeatM :: Monad m => Int -> (a -> m a) -> m a -> m a
repeatM n f = repeat n (>>= f)

snot :: Bool -> Q Bool
snot = rotate (pi / 2)

snot1 :: Int -> P Bool
snot1 n = pure True & repeatM n snot & observe
 
subtract :: V s => W s a -> W s a -> W s a
subtract a b = a <> ((negateS oneS) .* b)

-- | Quantum Zeno effect.
--
-- A watched pot never boils.
zeno1 :: Int -> P Bool
zeno1 n = pure True & repeatM n (rotate (pi / fromIntegral n)) & collect & observe

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

liftBind :: (Functor f, Monad g) => f (g a) -> (a -> g b) -> f (g b)
liftBind fga f = fmap (>>= f) fga

zeno2 :: Int -> P Bool
zeno2 n = pure True & repeat n
  (\x -> x `ffor` pure `liftBind` rotate (pi / fromIntegral n) `ffor` observe & join
  ) & collect

type MixedState a = P (Q a)

data Experimenter = Experimenter
  { experimenterMemory :: [Bool]
  , experimenterState :: !Bool
  }
  deriving (Eq, Ord)

zeno3 :: Int -> P Bool
zeno3 n = pure (Experimenter [] True) & repeatM n
  (\(Experimenter m s) -> do
    s' <- rotate (pi / fromIntegral n) s
    pure $ Experimenter (s:m) s' 
  ) & observe & fmap experimenterState & collect

trimZero :: V s => W s a -> W s a
trimZero = W . List.filter (\(Space _ v) -> not $ nearZeroS v) . runW

simplify :: Ord a => Q a -> Q a
simplify = trimZero . collect

