{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.Controller.Form.Forum
( formCreate
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans (MonadIO(..))
------------------------------------------------------------------------------
import qualified Text.Reform.Extra      as HA
import           Text.Reform                  ((<++), (++>))
import qualified Text.Reform.Blaze.Text as HA
import qualified Text.Blaze.Html as B (Html)
------------------------------------------------------------------------------
import           Data.Char
import qualified Data.Text      as TS
import qualified Data.Text.Lazy as TL
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.Controller.Form
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type.Forum as IF
------------------------------------------------------------------------------

formCreate :: (Functor m, MonadIO m)
           => Maybe IF.ForumID
           -> Form m B.Html () IF.ForumData
formCreate fparent = construct
    <$> HA.label ("Forum Title: " :: String)  
        ++> forumTitle
        <++ HA.br
    <*> HA.label ("Forum Slug Segment: " :: String)
        ++> forumPath
        <++ HA.br
    <*> HA.label ("Forum Description: " :: String)
        ++> forumDescription
        <++ HA.br
    <* HA.inputSubmit "Create Forum"
    where
      construct t p d = IF.ForumData
        { IF.forumParent = fparent
        , IF.forumPathSegment = p
        , IF.forumTitle = t
        , IF.forumDescription = d
        }

forumDescription :: (Functor m, Monad m)
                  => Form m B.Html () Content
forumDescription = content . TL.fromChunks . (:[]) <$>
    HA.checkBool (\x -> let l = TS.length x in l >= 0 && l <= 10000) (FERequiredLength 0 10000) (
    HA.textarea 80 10 "" 
    )

forumTitle :: (Functor m, Monad m)
          => Form m B.Html () TS.Text
forumTitle = HA.checkBool (\x -> let l = TS.length x in l > 1 && l < 20) (FERequiredLength 1 20) $
    HA.inputText ""

-- | TODO: Check uniqueness.
forumPath :: (Functor m, Monad m)
          => Form m B.Html () PathSegment
forumPath = PathSegment <$>
    HA.checkBool (\x -> let l = TS.length x in l > 1 && l < 15) (FERequiredLength 1 15) (
    HA.checkBool (TS.all allowedChar) FERequiredUnique ( -- ^ TODO: RequiredCharset
    HA.inputText ""
    ))
    where
      allowedChar c = isAlphaNum c || c == '_'
