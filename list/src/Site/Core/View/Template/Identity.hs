{-# LANGUAGE QuasiQuotes       #-}
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
import           Template.HSML
------------------------------------------------------------------------------
import qualified Happstack.Identity as HA
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Route.Type                  as I
import qualified Site.Core.Route.Type             as IC
import qualified Site.Core.Model.Type.Profile     as IC
import qualified Site.Core.Model.Type.Identity    as IC
import qualified Site.Core.Model.Type.Credentials as IC
------------------------------------------------------------------------------

templateUpdate :: B.Html -- ^ The form.
               -> [(IC.CredentialsID, HA.Credentials)]
               -> DefaultTemplate
templateUpdate form credentials = def
    { templateSectionM = [middle] 
    , templateTitle = "Identity"
    }
    where
      middle = [m|
        <h2>Identity</h2>
        <h3>Profile</h3>
        {h|form|}
        <h3>Credentials</h3>
        <table class="table table-bordered table-hover table-condensed">
          <thead>
            <tr>
              <th>Credentials Type</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody>
            {h|mapM_ credentialsCell credentials|} 
          </tbody>
        </table>
        |]

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

credentialsCell :: (IC.CredentialsID, HA.Credentials)
                -> B.Markup
credentialsCell (cid, HA.CredentialsPassword _) = [m|
  <tr>
    <td>Password</td>
    <td><a class="btn btn-mini" href={h|route $ I.Core $ IC.CredentialsUpdate cid|}>Update</a></td>
  </tr> |]





