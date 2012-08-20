{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.Controller.Form.Post
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
import qualified Data.Text as TS 
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.Controller.Form
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type  as IC
import qualified Site.Forum.Model.Type as IF
import qualified Site.Forum.Model.Forum as IF.Forum
------------------------------------------------------------------------------

formCreate :: (Functor m, MonadIO m, IF.HasForum m)
           => Maybe IC.IdentityID
           -> UTCTime
           -> Form m B.Html () IF.Post
formCreate mauthor time = construct
    <$> HA.label ("Title: " :: String)
        ++> postTitle
        <++ HA.br
    <*> HA.label ("Content: " :: String)
        ++> postContent
        <++ HA.br
    <*> HA.label ("Forums: " :: String)
        ++> postForums 
        <++ HA.br
    <*> HA.label ("Anonymous: " :: String)
        ++> HA.inputCheckbox False 
        <++ HA.br
    <* HA.inputSubmit "Create Post"
    where
      construct title content forums anon = IF.Post 
        { IF.postAuthor = if anon then Nothing else mauthor
        , IF.postTitle = title
        , IF.postContent = content
        , IF.postForums = forums
        , IF.postCreated = time
        }

postContent :: (Functor m, Monad m)
            => Form m B.Html () TS.Text
postContent =
    HA.checkBool (\x -> let l = TS.length x in l >= 10 && l <= 20000) (FERequiredLength 10 20000) $
    HA.textarea 80 40 "" 

postTitle :: (Functor m, Monad m)
          => Form m B.Html () TS.Text
postTitle = 
    HA.checkBool (\x -> let l = TS.length x in l >= 2 && l <= 100) (FERequiredLength 2 100) $
    HA.inputText "" 

postForums :: (Functor m, Monad m, IF.HasForum m)
           => Form m B.Html () [IF.ForumID]
postForums = 
    HA.checkM checker $
    HA.inputText ""
    where
      checker raw = emap FENonexistentForums id <$> 
        foldM (\acc x -> go acc $ TS.strip x) (Right []) (TS.split (==',') raw)         
      
      go (Left names) name = do 
        mfid <- lookupByName name
        case mfid of
            Nothing -> return $ Left $ name : names
            Just  _ -> return $ Left names
      
      go (Right fids) name = do
        mfid <- lookupByName name
        case mfid of
            Just fid -> return $ Right $ fid : fids
            Nothing  -> return $ Left [name]

      lookupByName name = 
        case textToPath name of
            Just path -> fmap key <$> IF.Forum.lookupByPath path
            Nothing   -> return Nothing

      emap l r x = case x of
                       Left y  -> Left  $ l y
                       Right y -> Right $ r y 
