{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.View.Template.Comment
( templateCreate
, templateRead
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Text.Blaze.Html as B
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
templateRead pid (cid, IF.Comment IF.CommentData{..} _) = [m|
  <div class="well well-small comment">
    <div>{h|commentContent|}</div>
    <a class="btn btn-mini" href={h|route $ I.Forum $ IF.CommentCreate pid $ Just cid|}>Reply</a>
  </div> |]

