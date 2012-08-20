{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.View.Template.Forum
( templateList

, templateRead
, templateCreate
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
import qualified Site.Forum.View.Template.Post as IF.Post
------------------------------------------------------------------------------

templateList :: [IF.ForumEntity]
             -> DefaultTemplate
templateList flist = def 
    { templateSectionM = [middle]
    , templateSectionR = [right]
    , templateTitle = "Forum List"
    }
    where
      middle = B.table ! B.class_ "table table-striped table-bordered" $ do
        B.thead $ B.tr $ B.th "Forum Name"  
        B.tbody $ mapM_ forumCell flist
      right  = B.a ! B.href (route $ I.Forum IF.ForumCreate) $ "Create Forum" 

templateRead :: IF.ForumEntity
             -> [IF.PostEntity]
             -> DefaultTemplate
templateRead (_, IF.Forum{..}) plist = def
    { templateSectionM = [middle]
    , templateSectionR = [right]
    , templateTitle = forumName <> " :: Forum"
    }
    where
      middle = B.table ! B.class_ "table table-striped table-bordered" $ do
        B.thead $ B.tr $ B.th "Post Title"
        B.tbody $ mapM_ IF.Post.postCell plist
      right  = 
        B.ul $ do
          B.li $ B.a ! B.href (route $ I.Forum IF.PostCreate) $ "Create Post"
          B.li $ B.a ! B.href (route $ I.Forum $ IF.ForumCreateSub forumPath) $ "Create Sub-Forum"

templateCreate :: B.Html
               -> DefaultTemplate
templateCreate view = def 
    { templateSectionM = [middle]
    , templateSectionR = [right]
    , templateTitle = "Forum Creation"
    }
    where
      middle = do
        B.h2 "Forum Creation"
        view

      right  = B.a ! B.href (route $ I.Forum IF.ForumCreate) $ "Create Forum" 

forumCell :: IF.ForumEntity -> B.Html
forumCell (_, IF.Forum{..}) =  
    B.tr $ B.td $ B.a ! B.href (route $ I.Forum $ IF.ForumReadFrontPage forumPath) $ B.toHtml forumName

