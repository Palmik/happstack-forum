{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.View.Template.Comment
( templateCreate
, templateRead
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Text.Blaze ((!))
import qualified Text.Blaze.Html             as B
import qualified Text.Blaze.Html5            as B 
import qualified Text.Blaze.Html5.Attributes as B hiding (title)
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Route.Type as I
import qualified Site.Forum.Route.Type as IF
import qualified Site.Forum.Model.Type as IF
------------------------------------------------------------------------------

templateCreate :: B.Html
               -> DefaultTemplate
templateCreate view = def
    { templateTitle = "Create Comment"
    , templateSectionM = [middle]
    }
    where
      middle = view

templateRead :: IF.PostID
             -> IF.CommentEntity
             -> B.Html
templateRead pid (cid, IF.Comment{..}) = 
    B.div $ do
      B.div ! B.class_ "well" $ B.toHtml commentContent
      B.a ! B.href (route $ I.Forum $ IF.CommentCreate pid $ Just cid) $ "Reply"

