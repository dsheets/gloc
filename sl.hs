import Data.Map as Map
import Control.Monad.State

class SLSymantics repr where
      int :: Int -> repr Int
      bool :: Bool -> repr Bool
      float :: Float -> repr Float
      tup2:: (repr a, repr b) -> repr (a, b)
      lam :: (repr a -> repr b) -> repr (a -> b)
      app :: repr (a -> b) -> repr a -> repr b
      fix :: (repr a -> repr a) -> repr a
      and :: repr Bool -> repr Bool -> repr Bool
      or  :: repr Bool -> repr Bool -> repr Bool
      add :: repr Int -> repr Int -> repr Int
      sub :: repr Int -> repr Int -> repr Int
      mul :: repr Int -> repr Int -> repr Int
      div :: repr Int -> repr Int -> repr Int
      addf:: repr Float -> repr Float -> repr Float
      subf:: repr Float -> repr Float -> repr Float
      mulf:: repr Float -> repr Float -> repr Float
      divf:: repr Float -> repr Float -> repr Float
      mod :: repr Int -> repr Int -> repr Int
      modf:: repr Float -> repr Float -> repr Float
      sqrt_:: repr Float -> repr Float
      leq :: repr Int -> repr Int -> repr Bool
      leqf:: repr Float -> repr Float -> repr Bool
      if_ :: repr Bool -> repr a -> repr a -> repr a

newtype Bind = Bind { name :: String } deriving (Eq, Ord, Show)

define f expr = modify $ insert f expr

csqrt :: SLSymantics repr => State (Map Bind (repr (Float -> (Float, Float)))) ()
csqrt = define (Bind "csqrt") $ lam
               (\x -> if_ (leqf (float 0.0) x)
                          (tup2 (sqrt_ x, float 0.0))
                          (tup2 (float 0.0, sqrt_ (mulf (float (-1.0)) x))))

sl :: State (Map Bind (repr a)) () -> Map Bind (repr a)
sl f = execState f empty

quadroots :: SLSymantics repr => Map Bind (repr (Float -> (Float, Float)))
quadroots = sl $ do csqrt
                 
