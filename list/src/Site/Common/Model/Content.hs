{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Common.Model.Content
( Content
, content
, getContent
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Data.Data
import qualified Data.SafeCopy  as SC
import qualified Data.Text.Lazy as TL
------------------------------------------------------------------------------
import qualified Text.Blaze    as B
import qualified Text.Markdown as M
------------------------------------------------------------------------------

newtype Content = Content
    { contentBody :: TL.Text
    } deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy)
-- $(SC.deriveSafeCopy 0 'SC.base ''Content)

-- | Creates a content from Markdown formated text.
content :: TL.Text -> Content
content = Content
{-# INLINE content #-}

-- | Gives you a Markdown formated text that is the base of this content.
getContent :: Content -> TL.Text
getContent = contentBody
{-# INLINE getContent #-}

instance B.ToMarkup Content where
    toMarkup = M.markdown M.def . contentBody


