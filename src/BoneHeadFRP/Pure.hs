
{- | This is the pure implementation of the FRP system. It assumes behaviors are
    functions from time and events are streams of time, value pairs.
  -}

module BoneHeadFRP.Pure where

import Control.Monad (ap, liftM, join)
import Control.Monad.Fix


type Time = Double


newtype Behavior a = B { at :: Time -> a }

instance Functor Behavior where
  fmap = liftM

instance Applicative Behavior where
  pure = return
  (<*>) = ap

instance Monad Behavior where
  return x = B $ \_ -> x

  (>>=) (B f) g = B $ \t -> at (g (f t)) t

instance MonadFix Behavior where
  mfix f = B $ \t -> let x = at (f x) t in x

time :: Behavior Time
time = B id

integrate :: Fractional a => Integer -> Time -> Behavior a -> Behavior a
integrate n t0 (B f) = B $ \t -> if t <= t0 then 0 else
  let delta = (t - t0)/(fromInteger n)
      delta' = fromRational $ toRational delta
      ts = [t0, (t0 + delta) .. t]
  in
    sum $ map (\t' -> delta' * (f t')) ts



newtype Event a = E { occurrences :: [(Time, a)] }

instance Functor Event where
  fmap f (E xs) = E $ fmap (\(t, x) -> (t, f x)) xs


merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge f (E xs) (E ys) = E $ merge' xs ys
  where
    merge' ((t1, x):xs) ((t2, y):ys)
      | t1 < t2 = (t1, x) : (merge' xs ((t2, y):ys))
      | t2 < t1 = (t2, y):(merge' ((t1, x):xs) ys)
      | otherwise = (t1, f x y) : (merge' xs ys)

with :: (a -> b -> c) -> Event a -> Behavior b -> Event c
with f (E xs) (B g) = E $ map (\(t, x) -> (t, f x (g t))) xs

hold :: a -> Event a -> Behavior a
hold x (E xs) = B $ \t -> case getLast t Nothing xs of
                            Nothing -> x
                            Just y -> y
  where
    getLast t m [] = m
    getLast t m ((t', x) : xs) = if t' <= t then getLast t (Just x) xs else m

untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e = join (hold b e)


takeE :: Int -> Event a -> Event a
takeE n (E xs) = E $ take n xs

dropE :: Int -> Event a -> Event a
dropE n (E xs) = E $ drop n xs

filterE :: (a -> Bool) -> Event a -> Event a
filterE f (E xs) = E $ filter (\(_, x) -> f x) xs

scanE :: (b -> a -> b) -> b -> Event a -> Event b
scanE f x (E xs) = case xs of
                     [] -> E []
                     ((t, v):xs') -> E $ scanE' t (f x v) xs'
  where
    scanE' t x [] = [(t, x)]
    scanE' t1 x ((t2, y):xs) = (t1, x) : (scanE' t2 (f x y) xs)

