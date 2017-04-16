module Data.Optimizely.Internal where

import Prelude
import Data.DateTime as D
import Data.List as L
import Data.List.NonEmpty as NEL
import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (throwError, withExcept)
import Data.Array (fromFoldable, toUnfoldable)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foldable (class Foldable)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt, readString, toForeign)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Null (Null)
import Data.Foreign.Undefined (Undefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Traversable (sequence, traverse)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL)
import Partial.Unsafe (unsafePartialBecause)


consNonEmptyWithList :: forall a. a -> List a -> NonEmptyList a
consNonEmptyWithList x xs = NonEmptyList $ x :| xs

consNonEmpty :: forall a. a -> NonEmptyList a -> NonEmptyList a
consNonEmpty x (NonEmptyList (NonEmpty h lst)) = NonEmptyList $ x :| h : lst

zipWith :: forall a b c. (a -> b -> c) -> NonEmptyList a -> NonEmptyList b -> NonEmptyList c
zipWith f (NonEmptyList (NonEmpty x xs)) (NonEmptyList (NonEmpty y ys)) = consNonEmptyWithList (f x y) (L.zipWith f xs ys)

range :: Int -> Int -> NonEmptyList Int
range start end = consNonEmptyWithList start $ L.range (start + 1) end

mapWithIndex :: forall a b. (Int -> a -> b) -> NonEmptyList a -> NonEmptyList b
mapWithIndex f (NonEmptyList (NonEmpty h lst)) = consNonEmptyWithList (f 0 h) $ L.reverse $ go 1 lst Nil
    where
    go _ Nil acc = acc
    go n (x : xs) acc = go (n+1) xs (f n x : acc)

reverse :: forall a. NonEmptyList a -> NonEmptyList a
reverse (NonEmptyList (NonEmpty h lst)) = unsafePartialBecause "We are uncons-ing a NonEmptyList" $ reCons $ L.uncons $ L.reverse $ h : lst
    where
    reCons :: (Partial) => Maybe { "head" :: a , "tail" :: List a } -> NonEmptyList a
    reCons (Just {head, tail}) = (NonEmptyList (NonEmpty head tail))

readBoundedEnum :: forall a. BoundedEnum a => Foreign -> F a
readBoundedEnum value = parseType =<< readInt value
    where
        parseType val = maybe' (error val) pure $ toEnum val
        error val _ = fail $ ForeignError $ "Expected a number between " <> bottomIndex <> " and " <> topIndex <> ", found " <> show val
        bottomIndex = show $ fromEnum (bottom :: a)
        topIndex = show $ fromEnum (top :: a)
