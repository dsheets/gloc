import Data.Map as Map
import Control.Monad.State

class SLSymantics repr where
      int :: Int -> repr Int
      bool :: Bool -> repr Bool
      float :: Float -> repr Float
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
      leq :: repr Int -> repr Int -> repr Int
      leqf:: repr Float -> repr Float -> repr Float
      if_ :: repr Bool -> repr a -> repr a -> repr a

newtype Function = Function { name :: String } deriving (Eq, Ord, Show)

define f expr = modify $ insert

quadform = Function "quadform"
quadroots = sl $ do
          define quadform $
                 
