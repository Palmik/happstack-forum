{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Common.View.Template
( DefaultTemplate(..)
, defaultTemplate

, route
, icon
, entity

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server   as HA
import qualified Happstack.Identity as HA
------------------------------------------------------------------------------
import qualified Web.Routes.PathInfo as WR
------------------------------------------------------------------------------
import           Text.Blaze ((!))
import qualified Text.Blaze.Internal         as B (preEscapedText)
import qualified Text.Blaze.Html             as B
import qualified Text.Blaze.Html5            as B 
import qualified Text.Blaze.Html5.Attributes as B hiding (title)
------------------------------------------------------------------------------
import qualified Site.Route.Type       as I
import qualified Site.Core.Route.Type  as IC
import qualified Site.Forum.Route.Type as IF
------------------------------------------------------------------------------
import           Template.HSML as Export
------------------------------------------------------------------------------

data DefaultTemplate = DefaultTemplate
    { templateTitle :: Text
    , templateSectionHead :: [B.Html]
    , templateSectionL :: [B.Html]
    , templateSectionM :: [B.Html]
    , templateSectionR :: [B.Html]
    }

instance Default DefaultTemplate where
    def = DefaultTemplate
            { templateTitle = "Untitled"
            , templateSectionHead = []
            , templateSectionL = []
            , templateSectionM = []
            , templateSectionR = []
            }

defaultTemplate :: (HA.Happstack m, HA.HasIdentityManager m)
                => DefaultTemplate
                -> m B.Html
defaultTemplate args = do
    signedIn <- HA.hasSessionCookie 
    return $! rawDefaultTemplate signedIn args

rawDefaultTemplate :: Bool -> DefaultTemplate -> B.Html
rawDefaultTemplate signedIn DefaultTemplate{..} = do
  B.docType
  B.html $ do
    B.head $ do
      B.title $ B.toHtml templateTitle
      B.meta ! B.httpEquiv "Content-Type"     ! B.content "text/html;charset=utf-8"
      B.meta ! B.httpEquiv "Content-Language" ! B.content "en-gb"
      B.meta ! B.httpEquiv "Author"           ! B.content "Petr Pilař"
      B.meta ! B.httpEquiv "Robots"           ! B.content "index,follow"
      
      B.link   ! B.rel "stylesheet" ! B.type_ "text/css" ! B.href "/static/styles/style.css"
      sequence_ templateSectionHead

    B.body $ do
      B.div ! B.class_ "navbar navbar-fixed-top" $ 
        B.div ! B.class_ "navbar-inner" $ 
          B.div ! B.class_ "container-fluid" $ do
            B.a ! B.class_ "brand" ! B.href "#" $ "Project name"
            B.div ! B.class_ "nav-collapse" $ 
              B.ul ! B.class_ "nav" $ do
                B.li $ B.a ! B.href (route $ I.Core  IC.Home) $ "Home"
                B.li $ B.a ! B.href (route $ I.Forum IF.Home) $ "Forums"

      B.div ! B.class_ "container-fluid" $ do
        B.div ! B.class_ "row-fluid" $ do
          B.div ! B.class_ "span3" $ do
            B.div ! B.class_ "row-fluid" $
              B.div ! B.class_ "well span12 nav-sidebar" $ 
                B.ul ! B.class_ "nav nav-list" $ do
                  B.li $ B.a ! B.href (route $ I.Core  IC.Home)   $ icon "home"    <> " Home"
                  B.li $ B.a ! B.href (route $ I.Forum IF.Home)   $ icon "comment" <> " Forums"
                  if signedIn
                     then do 
                       B.li $ B.a ! B.href (route $ I.Core IC.Signout) $ icon "off" <> " Sign out"
                       B.li $ B.a ! B.href (route $ I.Core IC.IdentitySelfUpdate) $ icon "user" <> " Identity"
                     else
                       B.li $ B.a ! B.href (route $ I.Core  IC.Signin)  $ icon "off" <> " Sign in"
          
            B.div ! B.class_ "row-fluid" $
              B.div ! B.class_ "well span12" $ 
                sequence_ templateSectionL

          B.div ! B.class_ "span6" $ 
            sequence_ templateSectionM

          B.div ! B.class_ "span3" $
            B.div ! B.class_ "well span12" $ 
              sequence_ templateSectionR

        B.hr

        B.footer $ B.p $ entity "copy" <> " Petr Pilař 2012."

      B.script ! B.src "/static/scripts/bootstrap.js" $ ""

------------------------------------------------------------------------------
-- | CONVENIENCE FUNCTIONS

route :: WR.PathInfo url => url -> B.AttributeValue
route = B.toValue . WR.toPathInfo

entity :: Text -> B.Html
entity ent = B.preEscapedText "&" <> B.toHtml ent <> B.preEscapedText ";"

icon :: B.AttributeValue -> B.Html
icon ico = B.i ! B.class_ ("icon-" <> ico) $ ""
