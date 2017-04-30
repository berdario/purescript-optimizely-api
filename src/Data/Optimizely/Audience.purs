module Data.Optimizely.Audience where

import Prelude
import Data.List.NonEmpty as NEL
import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Array (fromFoldable, toUnfoldable)
import Data.DateTime.Foreign (DateTime)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum, fromEnum)
import Data.Foldable (class Foldable)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, toForeign)
import Data.Foreign.Class (class IsForeign, read, readJSON, class AsForeign, write)
import Data.Foreign.Generic (readGeneric, toForeignGeneric)
import Data.Foreign.Undefined (Undefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (sequence)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax.Request (class Requestable)

import Data.Optimizely.Project (Project)
import Data.Optimizely.Common (Account, Id, foreignOptions, foreignToRequest)
import Data.Optimizely.Internal (mapWithIndex, readBoundedEnum, consNonEmptyWithList, consNonEmpty)
import Data.Optimizely.Audience.Internal as Internal

foreignsError :: forall t. Foldable t => String -> t Foreign -> ForeignError
foreignsError errString fs = ForeignError $ errString <> (unsafeStringify $ fromFoldable fs)

data Condition
    = Not Condition
    | And (NonEmptyList Condition)
    | Or (NonEmptyList Condition)
    | Rule Internal.RuleRecord

instance showCondition :: Show Condition where
    show (Not c) = "(Not " <> show c <> ")"
    show (And cs) = "(And " <> show cs <> ")"
    show (Or cs) = "(Or " <> show cs <> ")"
    show (Rule r) = show $ Internal.Rule r


instance isForeignCondition :: IsForeign Condition where
    read val = do
        arrOrRule <- (Left <$> readArray val) <|> ((\(Internal.Rule r) -> Right $ Rule r) <$> read val)
        readCondition arrOrRule


readCondition :: Either (Array Foreign) Condition -> F Condition
readCondition (Left arr) = readConditionExpr $ toUnfoldable arr
readCondition (Right condition) = pure condition

readConditionExpr :: List Foreign -> F Condition
readConditionExpr (foreignOp : c : cs) = do
    op <- read foreignOp
    readConditionExpr' op $ consNonEmptyWithList c cs
readConditionExpr lst = fail $ foreignsError "List must start with 'and', 'or' or 'not' and have operands, but it was " lst

readAtIndex :: forall a. IsForeign a => Int -> Foreign -> F a
readAtIndex idx = withExcept (map (ErrorAtIndex idx)) <<< read

traverseRead :: forall a. IsForeign a => NonEmptyList Foreign -> F (NonEmptyList a)
traverseRead = sequence <<< mapWithIndex readAtIndex

readConditionExpr' :: String -> NonEmptyList Foreign -> F Condition
readConditionExpr' "not" (NonEmptyList (NonEmpty foreignCond Nil)) = Not <$> read foreignCond
readConditionExpr' "not" lst = fail $ foreignsError "Not expression should have only 1 condition argument, but the arguments were " lst
readConditionExpr' "and" lst = And <$> traverseRead lst
readConditionExpr' "or" lst = Or <$> traverseRead lst
readConditionExpr' other lst = fail $ foreignsError "Condition operator should be 'and', 'or' or 'not', but the Condition list was " $ consNonEmpty (toForeign other) lst

instance asForeignCondition :: AsForeign Condition where
    write (Not c) = write [write "not", write c]
    write (And cs) = write $ fromFoldable (write "and" `consNonEmpty` map write cs)
    write (Or cs) = write $ fromFoldable (write "or" `consNonEmpty` map write cs)
    write (Rule r) = write $ Internal.Rule r

newtype RootCondition = RootCondition Condition

derive newtype instance showRootCondition :: Show RootCondition

instance isForeignRootCondition :: IsForeign RootCondition where
    read val = do
        nestedJSON <- readString val
        lst <- toUnfoldable <$> readJSON nestedJSON
        RootCondition <$> readConditionExpr lst

nestedWrite :: forall a. AsForeign a => a -> Foreign
nestedWrite = write <<< unsafeStringify <<< write

instance asForeignRootCondition :: AsForeign RootCondition where
    write (RootCondition (Rule r)) = nestedWrite $ And $ NEL.singleton (Rule r)
    write (RootCondition cond) = nestedWrite cond

newtype Audience = Audience
    { description :: String
    , project_id :: Id Project
    , id :: Id Audience
    , name :: String
    , created :: DateTime
    , conditions :: RootCondition
    , last_modified :: DateTime
    , segmentation :: Boolean
    , archived :: Boolean
    }
derive instance genericAudience :: Generic Audience _
derive instance newtypeAudience :: Newtype Audience _

instance foreignAudience :: IsForeign Audience where
    read = readGeneric foreignOptions

instance showAudience :: Show Audience where
    show = genericShow

type EditAudience name =
    { name :: name
    , description :: Undefined String
    , conditions :: Undefined RootCondition
    , segmentation :: Undefined Boolean
    }

type NewAudience = EditAudience String
type PutAudience = EditAudience (Undefined String)

newtype MkNewAudience = MkNewAudience NewAudience
derive instance genericNewAudience :: Generic MkNewAudience _

instance asForeignNewAudience :: AsForeign MkNewAudience where
    write = toForeignGeneric foreignOptions

instance requestableNewAudience :: Requestable MkNewAudience where
    toRequest = foreignToRequest

newtype MkPutAudience = MkPutAudience PutAudience
derive instance genericPutAudience :: Generic MkPutAudience _

instance asForeignPutAudience :: AsForeign MkPutAudience where
    write = toForeignGeneric foreignOptions

instance requestablePutAudience :: Requestable MkPutAudience where
    toRequest = foreignToRequest



data ListType = Cookie | QueryParameter | ZipCode
derive instance eqListType :: Eq ListType
derive instance ordListType :: Ord ListType
derive instance genericListType :: Generic ListType _

instance boundedListType :: Bounded ListType where
  bottom = Cookie
  top = ZipCode

instance enumListType :: Enum ListType where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumListType :: BoundedEnum ListType where
  cardinality = Cardinality 3
  toEnum = case _ of
    1 -> Just Cookie
    2 -> Just QueryParameter
    3 -> Just ZipCode
    _ -> Nothing
  fromEnum = case _ of
    Cookie -> 1
    QueryParameter -> 2
    ZipCode -> 3


instance foreignListType :: IsForeign ListType where
    read = readBoundedEnum

instance asForeignListType :: AsForeign ListType where
    write = write <<< fromEnum

instance showListType :: Show ListType where
    show = genericShow


newtype TargetingList = TargetingList
    { name :: Internal.ListName
    , description :: String
    , list_type :: ListType
    , key_fields :: String
    , id :: Id TargetingList
    , project_id :: Id Project
    , account_id :: Id Account
    , format :: String
    }
derive instance genericTargetingList :: Generic TargetingList _
derive instance newtypeTargetingList :: Newtype TargetingList _

instance foreignTargetingList :: IsForeign TargetingList where
    read = readGeneric foreignOptions

instance showTargetingList :: Show TargetingList where
    show = genericShow

type EditTargetingList =
    { name :: Internal.ListName
    , list_type :: ListType
    , list_content :: String
    , description :: Undefined String
    }

newtype MkEditTargetingList = MkEditTargetingList
    { name :: Internal.ListName
    , list_type :: ListType
    , list_content :: String
    , description :: Undefined String
    , format :: String
    }

mkEditTargetingList :: EditTargetingList -> MkEditTargetingList
mkEditTargetingList {name, list_type, list_content, description}
    = MkEditTargetingList
        { name:name
        , list_type:list_type
        , list_content:list_content
        , description:description
        , format:"csv"
        }

derive instance genericEditTargetingList :: Generic MkEditTargetingList _

instance asForeignEditTargetingList :: AsForeign MkEditTargetingList where
    write = toForeignGeneric foreignOptions

instance requestableEditTargetingList :: Requestable MkEditTargetingList where
    toRequest = foreignToRequest

newtype Dimension = Dimension
    { name :: String
    , last_modified :: DateTime
    , client_api_name :: String
    , id :: Id Dimension
    , description :: String
    }
derive instance genericDimension :: Generic Dimension _
derive instance newtypeDimension :: Newtype Dimension _

instance foreignDimension :: IsForeign Dimension where
    read = readGeneric foreignOptions

instance showDimension :: Show Dimension where
    show = genericShow

type EditDimension name =
    { name :: name
    , client_api_name :: Undefined String
    , description :: Undefined String
    }

type NewDimension = EditDimension String
type PutDimension = EditDimension (Undefined String)

newtype MkNewDimension = MkNewDimension NewDimension
derive instance genericNewDimension :: Generic MkNewDimension _

instance asForeignNewDimension :: AsForeign MkNewDimension where
    write = toForeignGeneric foreignOptions

instance requestableNewDimension :: Requestable MkNewDimension where
    toRequest = foreignToRequest

newtype MkPutDimension = MkPutDimension PutDimension
derive instance genericPutDimension :: Generic MkPutDimension _

instance asForeignPutDimension :: AsForeign MkPutDimension where
    write = toForeignGeneric foreignOptions

instance requestablePutDimension :: Requestable MkPutDimension where
    toRequest = foreignToRequest





