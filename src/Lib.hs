{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib (hasInt, NInt, OnlyInt, BoolAndInt, BoolOrInt, RecInt, NoInt) where


import           Data.Kind
import           Generics.SOP
import qualified GHC.Generics as GHC
-- we want to find the presence of Int type somewhere in type declaration

data OnlyInt = Int1 Int deriving (GHC.Generic)
newtype NInt = NInt Int deriving (GHC.Generic)
data BoolAndInt = Int2 Bool Int deriving (GHC.Generic)
data BoolOrInt = Bool Bool | Int Int deriving (GHC.Generic)
data RecInt = Rec RecInt | Leaf Int deriving (GHC.Generic)
data NoInt = Rec2 Bool | Leaf2 Bool deriving (GHC.Generic)

instance Generic OnlyInt
instance Generic NInt
instance Generic BoolOrInt
instance Generic BoolAndInt
instance Generic RecInt
instance Generic NoInt

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
    (++) a '[] = a
    (++) '[] b = b
    (++) (a ': as) bs = a ': (as ++ bs)


type family Flatten (xs :: [[Type]]) :: [Type] where
    Flatten '[] = '[]
    Flatten (x ': xs) = x ++ Flatten xs


type family HasInt (xs :: [Type]) :: Bool where
    HasInt '[ ] = 'False
    HasInt (Int ': _) = 'True
    HasInt (_ ': t) = HasInt t

class DemoteBool (a :: Bool) where
    demoteBool :: Bool

instance DemoteBool 'True where
    demoteBool = True

instance DemoteBool 'False where
    demoteBool = False


hasInt :: forall (x :: Type) (ys :: [[Type]]) (b :: Bool). (Code x ~ ys, b ~ HasInt (Flatten ys), DemoteBool b) => Bool
hasInt = demoteBool @b
