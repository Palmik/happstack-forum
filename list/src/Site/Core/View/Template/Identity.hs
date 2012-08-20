{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.View.Template.Identity
( templateRead
, templateUpdate
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Text.Blaze.Html  as B
import qualified Text.Blaze.Html5 as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import qualified Site.Core.Model.Type.Profile  as IC
import qualified Site.Core.Model.Type.Identity as IC
------------------------------------------------------------------------------

templateUpdate :: B.Html -- ^ The form.
               -> DefaultTemplate
templateUpdate pform = def { templateSectionM = [middle] 
                           , templateTitle = "Identity"
                           }
    where
      middle = do 
        B.h2 "Update Identity"
        B.h3 "Update Profile"
        pform

templateRead :: Maybe IC.Identity
             -> DefaultTemplate 
templateRead midentity = def { templateSectionM = [middle] 
                             , templateTitle = "Identity"
                             }
    where
      middle = do
        B.h2 "Identity"
        case midentity of
          Just (IC.Identity (Just profile)) ->
            templateProfile profile
          Just (IC.Identity Nothing) ->
            B.p "There is no profile associated with this identity."
          Nothing ->
            B.p "This identity does not exist."


templateProfile :: IC.Profile -> B.Html
templateProfile IC.Profile{..} = 
    B.ul $ 
      B.li $ "Name: " <> B.toHtml profileHandle

