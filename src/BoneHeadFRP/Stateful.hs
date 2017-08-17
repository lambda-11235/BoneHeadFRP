
module BoneHeadFRP.Stateful
  ( Time, DTime, Timer, makeTimer, getTime
  , Behavior, at, now, time
  , Event, occurrences, merge, with, hold, scanE, filterE
  , untilB, stateful
  ) where

import Control.Monad (ap, liftM, join)
import Data.IORef
import Data.Time.Clock.System


type Time = Double
type DTime = Time

newtype Timer = Timer { startTime :: SystemTime }

-- TODO: Update to use modern time library.
makeTimer :: IO Timer
makeTimer = do st <- getSystemTime
               return (Timer st)

getTime :: Timer -> IO Time
getTime (Timer t) = do t' <- getSystemTime
                       let secs1 = fromIntegral $ systemSeconds t
                           nanos1 = fromIntegral $ systemNanoseconds t
                           secs2 = fromIntegral $ systemSeconds t'
                           nanos2 = fromIntegral $ systemNanoseconds t'
                       return ((secs2 + nanos2/10^9) - (secs1 + nanos1/10^9))


-- | A time varying value. Note that `at` will return bogus values for future
-- times if the behavior depends on a `statful` event stream, as the future
-- values have not been instantiated yet.
newtype Behavior a = B { at :: Time -> IO a }

now :: Timer -> Behavior a -> IO a
now timer b = do t <- getTime timer
                 at b t

pureB :: (Time -> a) -> Behavior a
pureB f = B $ return . f

instance Functor Behavior where
  fmap = liftM

instance Applicative Behavior where
  pure = return
  (<*>) = ap

instance Monad Behavior where
  return x = B $ \_ -> return x

  (>>=) (B f) g = B $ \t -> do x <- f t
                               at (g x) t

time :: Behavior Time
time = B return

-- | Performs integration over some behavior using Riemann sums. The arguments
-- are as follows: the number of samples to take, the start time, and the
-- behavior to integrate over. All values before the start time default to 0.
integrate :: Fractional a => Integer -> Time -> Behavior a -> Behavior a
integrate n t0 (B f) = B $ \t -> if t <= t0 then return 0 else
  let delta = (t - t0)/(fromInteger n)
      delta' = fromRational $ toRational delta
      ts = [t0, (t0 + delta) .. t]
      area t' = do x <- f t'
                   return (delta' * x)
  in
    do areas <- mapM area ts
       return (sum areas)



-- | An event stream.
newtype Event a = E { occurrences :: IO [(Time, a)] }

pureE :: [(Time, a)] -> Event a
pureE xs = E $ return xs

instance Functor Event where
  fmap f (E xs) = E $ fmap (fmap (\(t, x) -> (t, f x))) xs


merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge f (E xs) (E ys) = E $ merge' <$> xs <*> ys
  where
    merge' ((t1, x):xs) ((t2, y):ys)
      | t1 < t2 = (t1, x) : (merge' xs ((t2, y):ys))
      | t2 < t1 = (t2, y):(merge' ((t1, x):xs) ys)
      | otherwise = (t1, f x y) : (merge' xs ys)

with :: (a -> b -> c) -> Event a -> Behavior b -> Event c
with f (E xs) (B g) = E $ do xs' <- xs
                             mapM (\(t, x) -> fmap (\y -> (t, f x y)) (g t)) xs'

hold :: a -> Event a -> Behavior a
hold x (E xs) = B $ \t -> do xs' <- xs
                             return $ case getLast t Nothing xs' of
                                        Nothing -> x
                                        Just y -> y
  where
    getLast t m [] = m
    getLast t m ((t', x) : xs) = if t' <= t then getLast t (Just x) xs else m

-- Acts as the first behavior until the first event occurrence. After the first
-- event the behavior is given by the most recent event.
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b e = join (hold b e)


takeE :: Int -> Event a -> Event a
takeE n (E xs) = E $ fmap (take n) xs

dropE :: Int -> Event a -> Event a
dropE n (E xs) = E $ fmap (drop n) xs

filterE :: (a -> Bool) -> Event a -> Event a
filterE f (E xs) = E $ fmap (filter (\(_, x) -> f x)) xs

scanE :: (b -> a -> b) -> b -> Event a -> Event b
scanE f x (E xs) = E $ do xs' <- xs
                          return $
                            case xs' of
                              [] -> []
                              ((t, v):xs'') -> scanE' t (f x v) xs''
  where
    scanE' t x [] = [(t, x)]
    scanE' t1 x ((t2, y):ys) = (t1, x) : (scanE' t2 (f x y) ys)


-- | Creates a stateful event stream and a function to send new events to the
-- stream. All stateful event stream in an FRP system should use the same timer.
stateful :: Timer -> IO (Event a, a -> IO ())
stateful timer =
  do ref <- newIORef []
     let update v = do t <- getTime timer
                       modifyIORef ref (\xs -> xs ++ [(t, v)])
     return (E (readIORef ref), update)
