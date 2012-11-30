{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.View.Template.Forum
( templateListRead

, templateRead
, templateCreate
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Text.Blaze.Html as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Route.Type as I
import qualified Site.Forum.Route.Type as IF
import qualified Site.Forum.Model.Type as IF
import qualified Site.Forum.View.Template.Post as IF.Post
------------------------------------------------------------------------------

templateListRead :: [IF.ForumEntity]
                 -> DefaultTemplate
templateListRead flist = def 
    { templateSectionM = [middle]
    , templateSectionR = [right]
    , templateTitle = "Forum List"
    }
    where
      middle = [m|
        <table class="table table-striped table-bordered">
          <thead>
            <tr>
              <th>Forum Name</th>
            </tr>
          </thead> 
          <tbody>
            {h| mapM_ forumCell flist |}
          </tbody>        
        </table> |]
      
      right  = [m|
        <a class="btn btn-wide btn-wide" href={h|route $ I.Forum $ IF.ForumCreate PathRoot|}>Create Forum</a>
        |]

templateRead :: IF.ForumEntity
             -> [IF.PostEntity]
             -> DefaultTemplate
templateRead (_, IF.Forum IF.ForumData{..} IF.ForumDataExtra{..}) plist = def
    { templateSectionM = [middle]
    , templateSectionR = [right]
    , templateTitle = forumTitle <> " :: Forum"
    }
    where
      middle = [m|
        <h2>{h|forumTitle|}</h2>
        <table class="table table-striped table-bordered">
          <thead>
            <tr>
              <th>Post Title</th>
            </tr>
          </thead>
          <tbody>
            {h| mapM_ IF.Post.postCell plist |}
          </tbody>
        </table> |]
      
      right  = [m|
        <a class="btn btn-wide btn-wide" href={h|route $ I.Forum IF.PostCreate|}>Create Post</a>
        <a class="btn btn-wide btn-wide" href={h|route $ I.Forum $ IF.ForumCreate forumPath|}>Create Sub-Forum</a>
        <div>{h|forumDescription|}</div>
        |]

templateCreate :: B.Html
               -> DefaultTemplate
templateCreate view = def 
    { templateSectionM = [middle]
    , templateTitle = "Forum Creation"
    }
    where
      middle = [m|
        <h2>Forum Creation</h2>
        {h|view|} |]

forumCell :: IF.ForumEntity -> B.Html
forumCell (_, IF.Forum IF.ForumData{..} IF.ForumDataExtra{..}) = [m|
  <tr>
    <td><a href={h|route $ I.Forum $ IF.ForumRead forumPath PageFront|}>{h|forumTitle|}</a></td>
  </tr> |]

