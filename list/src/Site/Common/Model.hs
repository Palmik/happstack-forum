{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Common.Model
( Page(..)
, Order(..)
, AutoIncrementID(..)
, key
, value
, getPage

  -- * Some convenience wrappers

  -- * * Path
, Path(..)
, PathSegment(..)
, textToPath
, pathToText

, Natural(..)
, Parent(..)
, Ancestor(..)
, Title(..)

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.IxSet     as IX
import qualified Data.SafeCopy  as SC
import qualified Data.Text      as TS
import qualified Data.Text.Read as TS
------------------------------------------------------------------------------
import qualified Web.Routes as WR
import qualified Text.ParserCombinators.Parsec.Combinator as P (notFollowedBy)
------------------------------------------------------------------------------
import           Site.Common.Model.Content as Export
import           Data.Time as Export
------------------------------------------------------------------------------

-- | Only matches if all segments have been consumed
eof :: WR.URLParser ()
eof = P.notFollowedBy WR.anySegment
------------------------------------------------------------------------------
-- ENTITY FRAMEWORK

key :: (a, b) -> a
key = fst
{-# INLINE key #-}

value :: (a, b) -> b
value = snd
{-# INLINE value #-}

-- | To make your type and `Entity`, make it instance of this type-class.
class AutoIncrementID a where
    nextID :: a -> a
    initID :: a

------------------------------------------------------------------------------
-- CONVENIENCE TYPES

------------------------------------------------------------------------------
-- | Page
data Page = Page
   { pageNumber :: Int
   , pageSize :: Int
   } | PageFront
$(SC.deriveSafeCopy 0 'SC.base ''Page)

instance WR.PathInfo Page where
    toPathSegments (Page pn ps) = [TS.pack (show pn <> "-" <> show ps)] 
    toPathSegments PageFront = []
    fromPathSegments = pageFront <|> pageNormal
        where
          pageFront  = eof >> return PageFront
          pageNormal = do
            res <- map TS.decimal . TS.split (== '-') <$> WR.anySegment
            case res of
                [Right (pn, "")] -> return $ Page pn 30
                [Right (pn, ""), Right (ps, "")] -> return $ Page pn ps  
                _ -> fail "Page number and page size expected."

data Order = Increasing
           | Decreasing
    deriving (Data, Eq, Ord, Typeable, Show)
$(SC.deriveSafeCopy 0 'SC.base ''Order)

getPage :: forall k a . (IX.Indexable a, Typeable a, Typeable k)
        => IX.Proxy k
        -> Page
        -> Order
        -> IX.IxSet a
        -> [a]
getPage proxy PageFront ord = getPage proxy (Page 1 30) ord
getPage proxy (Page pn ps) ord =
    case ord of
         Increasing -> take ps . drop ((pn - 1) * ps) . IX.toAscList proxy
         Decreasing -> take ps . drop ((pn - 1) * ps) . IX.toDescList proxy
{-# INLINE getPage #-}


------------------------------------------------------------------------------
-- | Natural
newtype Natural = Natural { unNatural :: Int64 }
    deriving (Data, Eq, Ord, Typeable, Show)
$(SC.deriveSafeCopy 0 'SC.base ''Natural)

instance WR.PathInfo Natural where
    toPathSegments (Natural i) = [TS.pack $ show i]
    fromPathSegments = WR.pToken (const ("Natural" :: String)) check
        where
          check txt =
              case TS.decimal txt of
                   (Left _) -> Nothing
                   (Right (n, r))
                     | TS.null r -> Just $ Natural n
                     | otherwise -> Nothing

instance AutoIncrementID Natural where
    initID             = Natural 0
    nextID (Natural n) = Natural $! n + 1

------------------------------------------------------------------------------
-- Path
newtype PathSegment = PathSegment { unPathSegment :: TS.Text }
    deriving (Data, Eq, Ord, Typeable, SC.SafeCopy, Show)

data Path = Path { unPath :: [PathSegment] } | PathRoot
    deriving (Data, Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''Path)

instance Show Path where
    show = TS.unpack . TS.intercalate "-" . map unPathSegment . unPath

instance WR.PathInfo Path where
    toPathSegments PathRoot = []
    toPathSegments (Path p) = [ TS.intercalate "-" . map unPathSegment $ reverse p ]
    fromPathSegments = pathRoot <|> pathNormal
        where
          pathRoot = eof >> return PathRoot
          pathNormal = do
            res <- TS.split (== '-') <$> WR.anySegment
            if any TS.null res
               then fail "Empty path piece"
               else return $ Path $ reverse $ map PathSegment res

textToPath :: TS.Text -> Maybe Path
textToPath = tr . WR.parseSegments WR.fromPathSegments . (:[])
    where
      tr (Left _)  = Nothing
      tr (Right x) = Just x

pathToText :: Path -> TS.Text
pathToText = WR.toPathInfo

instance WR.PathInfo a => WR.PathInfo (Maybe a) where
    toPathSegments (Just x) = WR.toPathSegments x
    toPathSegments Nothing  = []    
    fromPathSegments = 
        (eof >> return Nothing) <|>
        (Just <$> WR.fromPathSegments)


------------------------------------------------------------------------------
-- Generic
newtype Parent    a = Parent { unParent :: a }
    deriving (Data, Eq, Ord, Typeable)

newtype Ancestor  a = Ancestor { unAncestor :: a }
    deriving (Data, Eq, Ord, Typeable)

newtype Title     a = Title { unTitle :: a }
    deriving (Data, Eq, Ord, Typeable)

