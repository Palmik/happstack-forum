{-# LANGUAGE QuasiQuotes       #-}
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
import qualified Text.Blaze.Html as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Route.Type as I
import qualified Site.Forum.Route.Type as IF
import qualified Site.Forum.Model.Type as IF
import qualified Site.Forum.View.Template.Comment as IF.Comment
------------------------------------------------------------------------------

templateRead :: IF.PostEntity
             -> [IF.CommentEntity]
             -> DefaultTemplate
templateRead (pid, IF.Post IF.PostData{..} IF.PostDataExtra{..}) comments = def
    { templateTitle = postTitle
    , templateSectionM = [middle]
    , templateSectionR = [right]
    }
    where
      middle = [m|
        <h2><a href={h|route $ I.Forum $ IF.PostRead pid PageFront|}>{h|postTitle|}</a></h2>
        <div>{h|postContent|}</div>
        <div class="comments">
          {h| mapM_ (IF.Comment.templateRead pid) comments |}
        </div> |]
      right = [m|
        <a class="btn btn-wide btn-wide" href={h|route $ I.Forum $ IF.CommentCreate pid Nothing|}>
          Comment
        </a> |]

templateCreate :: B.Html
               -> DefaultTemplate
templateCreate view = def
    { templateTitle = "Create Post"
    , templateSectionM = [middle]
    }
    where
      middle = view


postCell :: IF.PostEntity -> B.Html
postCell (pid, IF.Post IF.PostData{..} IF.PostDataExtra{..}) = [m|
  <tr>
    <td><a href={h|route $ I.Forum $ IF.PostRead pid PageFront|}>{h|postTitle|}</a></td>
  </tr> |]


