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
import qualified Data.Text as TS (Text, all, length)
import           Data.Char
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.Controller.Form
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type.Forum as IF
------------------------------------------------------------------------------

formCreate :: (Functor m, MonadIO m)
           => Maybe IF.ForumID
           -> UTCTime
           -> Form m B.Html () IF.Forum
formCreate fparent time = construct
    <$> HA.label ("Forum Name: " :: String)  
        ++> forumName
        <++ HA.br
    <*> HA.label ("Forum Path: " :: String)
        ++> forumPath
        <++ HA.br
    <* HA.inputSubmit "Create Forum"
    where
      construct fname fps = IF.Forum
        { IF.forumParent = fparent
        , IF.forumAncestors = []
        , IF.forumPathSegment = fps 
        , IF.forumPath = Path []
        , IF.forumName = fname
        , IF.forumCreated = time
        }

forumName :: (Functor m, Monad m)
          => Form m B.Html () TS.Text
forumName = HA.checkBool (\x -> let l = TS.length x in l > 1 && l < 20) (FERequiredLength 1 20) $
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
