{-| PropertyLens
 -
 - The goal here is to build a lens-like function that can pull a field out of a
 - record by following a path made up of property names. For example:
 -
 -    properties @'[ "a", "b" ] myRecord
 -
 - On a structure like { a: { b: 2 } } this should look up `a`, then `b`, and return 2.
-}


{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module PropertyLens
    ( run
    ) where

import GHC.TypeLits
import GHC.Generics


run :: IO ()
run = do
  putStrLn $ "Ex1 should be true: " ++ show example1
  putStrLn $ "Ex2 should be false: " ++ show example2


example1 :: Bool
example1 =
  let rec = RecA 1 ""
  in properties @"a1" rec == (1 :: Int)


example2 :: Bool
example2 =
  let rec = RecA 2 "hi"
  in properties @"a2" rec == "hello"


data RecA
  = RecA
    { a1 :: Int
    , a2 :: String
    }
  deriving Generic

newtype RecB = RecB { b :: RecC }
newtype RecC = RecC { c :: Int }


-----


-- THe base class, with the `properties` function we actually want to use
class PropertyLens (field :: Symbol) input out where
  properties :: input -> out

-- We will say that any type can be an instance of `PropertyLens` provided its generic
-- representation is an instance of `GPropertyLens`
class GPropertyLens (field :: Symbol) (rep :: * -> *) out where
  gproperties :: rep p -> out

instance (GPropertyLens field (Rep input) out, Generic input) => PropertyLens field input out where
  properties = gproperties @field . from

-- When walking the generic tree, we don't care about datatype or constructor metadata 
instance GPropertyLens field rest out => GPropertyLens field (M1 D meta rest) out where
  gproperties = gproperties @field . unM1

instance GPropertyLens field rest out => GPropertyLens field (M1 C meta rest) out where
  gproperties = gproperties @field . unM1

-- We do care about selector metadata: if the field name is the same as the one we're looking
-- and the types match up, we've found our value
instance GPropertyLens field (M1 S ('MetaSel ('Just field) x y z) (Rec0 out)) out where
  gproperties = unK1 . unM1

-- While walking the generic tree we might hit a product, in which case we have to work out
-- which branch to go down using the `ChooseBranch` type family
instance
  ( branch ~ ChooseBranch field out left right
  , ChosenBranch branch field out (left :*: right)
  ) => GPropertyLens field (left :*: right) out where
  gproperties = chosenBranch @branch @field @out

-- Go down the left or right branch as appropriate 
instance GPropertyLens field left out => ChosenBranch 'OnLeft field out (left :*: right) where
  chosenBranch (left :*: right) = gproperties @field left

instance GPropertyLens field right out => ChosenBranch 'OnRight field out (left :*: right) where
  chosenBranch (left :*: right) = gproperties @field right

-- We use this data type to indicate which branch to take
data Branch
  = OnLeft
  | OnRight

-- Work out which branch to take by 'looking ahead' to see which one will match
type family ChooseBranch (field :: Symbol) (out :: *) (left :: * -> *) (right :: * -> *) :: Branch where
  ChooseBranch field out (M1 S ('MetaSel ('Just field) _ _ _) (Rec0 out)) _ = 'OnLeft
  ChooseBranch field out _ (M1 S ('MetaSel ('Just field) _ _ _) (Rec0 out)) = 'OnRight

-- Once we've chosen a branch, we can use `chosenBranch` to step over it and continue
class ChosenBranch (branch :: Branch) (field :: Symbol) out (rep :: * -> *) where
  chosenBranch :: rep p -> out

