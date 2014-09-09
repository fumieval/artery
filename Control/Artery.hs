{-# LANGUAGE Rank2Types #-}
module Control.Artery (Artery(..)
    , runArtery
    , effectful
    , stateful
    , scan
    , scanM
    , fromList
    , runList
    , feedback
    , delay1
    , delay
    , cartridge
    , module Control.Arrow) where

import qualified Control.Category
import Control.Arrow
import Control.Applicative
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Profunctor
import Control.Monad.Trans.State
import Control.Concurrent
import Control.Monad.IO.Class

-- | 'Artery' is a device that produces a value from the input every beat.
newtype Artery m i o = Artery { unArtery :: forall r. i -> (o -> Artery m i o -> m r) -> m r }

instance Control.Category.Category (Artery m) where
    id = Artery $ \x cont -> cont x Control.Category.id
    Artery f . Artery g = Artery $ \x cont -> g x $ \y g' -> f y $ \z f' -> cont z (f' Control.Category.. g')

instance Arrow (Artery m) where
    arr f = let a = Artery $ \x cont -> cont (f x) a in a
    {-# INLINE arr #-}
    Artery f *** Artery g = Artery $ \(x, y) cont -> f x $ \x' f' -> g y $ \y' g' -> cont (x', y') (f' *** g')
    Artery f &&& Artery g = Artery $ \i cont -> f i $ \x f' -> g i $ \y g' -> cont (x, y) (f' &&& g')
    first (Artery f) = Artery $ \(x, y) cont -> f x $ \x' f' -> cont (x', y) (first f')
    second (Artery f) = Artery $ \(y, x) cont -> f x $ \x' f' -> cont (y, x') (second f')

instance ArrowChoice (Artery m) where
    left f = f +++ Control.Category.id
    {-# INLINE left #-}
    right f = Control.Category.id +++ f
    {-# INLINE right #-}
    f +++ g = Left <$> f ||| Right <$> g
    {-# INLINE (+++) #-}
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

instance Choice (Artery m) where
    left' = left
    {-# INLINE left' #-}
    right' = right 
    {-# INLINE right' #-}

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

instance Fractional o => Fractional (Artery m i o) where
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    recip = fmap recip
    fromRational = pure . fromRational

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

-- | Analogous to 'loop', but the feedback will be delayed a beat.
feedback :: r -> Artery m (i, r) (o, r) -> Artery m i o
feedback r (Artery v) = Artery $ \i cont -> v (i, r) $ \(o, r') v' -> cont o (feedback r' v')

-- | Delay a beat. The first argument is the default value for the output.
delay1 :: a -> Artery m a a
delay1 = scan const
{-# INLINE delay1 #-}

-- | 'delay n' propagates a signal n beat behind. 
delay :: Int -> a -> Artery m a a
delay n d = go (Seq.replicate n d) where
    go buf = Artery $ \i cont -> case Seq.viewl buf of
        a Seq.:< buf' -> cont a $ go $ buf' Seq.|> i
{-# INLINE delay #-}

fromList :: [a] -> Artery m b a
fromList seq = go seq where
    go (x:xs) = Artery $ \_ cont -> cont x (go xs)
    go [] = go seq

runList :: Applicative m => Artery m a b -> [a] -> m [b]
runList ar (x:xs) = unArtery ar x $ \y cont -> (y:) <$> runList cont xs
runList _ [] = pure []

cartridge :: MonadIO m => MVar (Artery m i o) -> Artery m i o
cartridge ref = go where
    go = Artery $ \i cont -> liftIO (takeMVar ref)
        >>= \a -> unArtery a i $ \o a' -> liftIO (putMVar ref a') >> cont o go