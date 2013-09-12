{-# LANGUAGE Rank2Types #-}
module Control.Artery (Artery(..)
    , runArtery
    , effectful
    , stateful
    , scan
    , scanM
    , feedback
    , delay1
    , delay
    , module Control.Arrow) where

import qualified Control.Category
import Control.Arrow
import Control.Applicative
import qualified Data.Vector as V
import Data.Monoid
import Data.Profunctor
import Control.Monad.Trans.State

-- | 'Artery' is a device that produces a value from the input every beat.
newtype Artery m i o = Artery { unArtery :: forall r. i -> (o -> Artery m i o -> m r) -> m r }

instance Control.Category.Category (Artery m) where
    id = Artery $ \x cont -> cont x Control.Category.id
    Artery f . Artery g = Artery $ \x cont -> g x $ \y g' -> f y $ \z f' -> cont z (f' Control.Category.. g')

instance Arrow (Artery m) where
    arr f = Artery $ \x cont -> cont (f x) (arr f)
    first (Artery f) = Artery $ \(x, y) cont -> f x $ \x' f' -> cont (x', y) (first f')
    second (Artery f) = Artery $ \(y, x) cont -> f x $ \x' f' -> cont (y, x') (second f')

instance ArrowChoice (Artery m) where
    left f = f +++ Control.Category.id
    right f = Control.Category.id +++ f
    f +++ g = Left <$> f ||| Right <$> g
    f ||| g = Artery $ \e cont -> case e of
        Left x -> unArtery f x $ \o f' -> cont o (f' ||| g)
        Right x -> unArtery g x $ \o g' -> cont o (f ||| g')

instance Functor (Artery m i) where
    fmap f = go where
        go (Artery v) = Artery $ \x cont -> v x $ \a v' -> cont (f a) (go v')
    {-# INLINE fmap #-}

instance Applicative (Artery m i) where
    pure x = go where
        go = Artery $ \_ cont -> cont x go
    {-# INLINE pure #-}
    Artery ff <*> Artery fx = Artery $ \i cont -> ff i $ \f ff' -> fx i $ \x fx' -> cont (f x) (ff' <*> fx')

instance Profunctor (Artery m) where
    dimap f g = go where
        go (Artery v) = Artery $ \i cont -> v (f i) $ \o v' -> cont (g o) (go v')
    {-# INLINE dimap #-}

instance Strong (Artery m) where
    first' = first
    {-# INLINE first' #-}
    second' = second
    {-# INLINE second' #-}

instance Num o => Num (Artery m i o) where
    (+) = liftA2 (+)
    {-# INLINE (+) #-}
    (-) = liftA2 (-)
    {-# INLINE (-) #-}
    (*) = liftA2 (*)
    {-# INLINE (*) #-}
    abs = fmap abs
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

instance Monoid o => Monoid (Artery m i o) where
    mempty = pure mempty
    {-# INLINE mempty #-}
    mappend = liftA2 mappend
    {-# INLINE mappend #-}

-- | Run the given action every beat.
effectful :: Monad m => (i -> m o) -> Artery m i o
effectful m = go where
    go = Artery $ \i cont -> m i >>= \o -> cont o go
{-# INLINE effectful #-}

-- | Run the given stateful action every beat.
stateful :: Monad m => (i -> StateT s m o) -> s -> Artery m i o
stateful m = go where
    go s = Artery $ \i cont -> runStateT (m i) s >>= \(o, s') -> cont o (go s')
{-# INLINE stateful #-}

-- | Produce values by accumulating inputs.
scan :: (i -> a -> a) -> a -> Artery m i a
scan f = go where
    go x = Artery $ \i cont -> cont x (go (f i x))
{-# INLINE scan #-}

-- | Analogous to 'scan', but it allows monadic accumulators.
scanM :: Monad m => (i -> a -> m a) -> a -> Artery m i a
scanM f = go where
    go x = Artery $ \i cont -> f i x >>= \a -> cont x (go a)
{-# INLINE scanM #-}

-- | Pump up the 'Artery'.
runArtery :: Monad m => Artery m i o -> i -> m (o, Artery m i o)
runArtery (Artery v) i = v i (curry return)
{-# INLINE runArtery #-}

-- | Analogous to 'loop', but the feedbacked signal is delayed a beat.
feedback :: r -> Artery m (i, r) (o, r) -> Artery m i o
feedback r (Artery v) = Artery $ \i cont -> v (i, r) $ \(o, r') v' -> cont o (feedback r' v')

-- | Delay a beat. The first argument is the default value of the output.
delay1 :: a -> Artery m a a
delay1 = scan const
{-# INLINE delay1 #-}

-- | 'delay n' propagates a signal n beat behind. 
delay :: Int -> a -> Artery m a a
delay n d = go 0 (V.replicate n d) where
    next ix
        | succ ix == n = 0
        | otherwise = succ ix
    go ix buf = Artery $ \i cont -> cont (buf V.! ix) (go (next ix) (V.unsafeUpd buf [(ix, i)]))
{-# INLINE delay #-}