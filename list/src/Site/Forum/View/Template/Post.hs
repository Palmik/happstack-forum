{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.View.Template.Post
( templateRead
, templateCreate
, postCell 
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
import qualified Site.Forum.View.Template.Comment as IF.Comment
------------------------------------------------------------------------------

templateRead :: IF.PostEntity
             -> [IF.CommentEntity]
             -> DefaultTemplate
templateRead (pid, IF.Post{..}) comments = def
    { templateTitle = postTitle
    , templateSectionM = [middle]
    }
    where
      middle = do
        B.h2 $ B.a ! B.href (route $ I.Forum $ IF.PostReadFrontPage pid) $ B.toHtml postTitle
        B.div $ B.toHtml postContent
        B.div $ mapM_ (IF.Comment.templateRead pid) comments


templateCreate :: B.Html
               -> DefaultTemplate
templateCreate view = def
    { templateTitle = "Create Post"
    , templateSectionM = [middle]
    }
    where
      middle = view


postCell :: IF.PostEntity -> B.Html
postCell (pid, IF.Post{..}) =
    B.tr $ B.td $ B.a ! B.href (route $ I.Forum $ IF.PostReadFrontPage pid) $ B.toHtml postTitle



