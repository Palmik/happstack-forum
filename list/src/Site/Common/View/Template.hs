{-# LANGUAGE RecordWildCards #-}

module Site.Common.View.Template
( DefaultTemplate(..)
, defaultTemplate
) where

------------------------------------------------------------------------------
import           Common
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

data DefaultTemplate = DefaultTemplate
    { templateTitle :: Text
    , templateSectionHead :: [B.Html]
    , templateSectionL :: [B.Html]
    , templateSectionM :: [B.Html]
    , templateSectionR :: [B.Html]
    }

defaultTemplate :: DefaultTemplate -> B.Html
defaultTemplate DefaultTemplate{..} = do
  B.docType
  B.html $ do
    B.head $ do
      B.title $ B.toHtml templateTitle
      B.meta ! B.httpEquiv "Content-Type"     ! B.content "text/html;charset=utf-8"
      B.meta ! B.httpEquiv "Content-Language" ! B.content "en-gb"
      B.meta ! B.httpEquiv "Author"           ! B.content "Petr Pilař"
      B.meta ! B.httpEquiv "Robots"           ! B.content "index,follow"
      
      B.link   ! B.rel "stylesheet" ! B.type_ "text/css"        ! B.href "/static/styles/bootstrap.css"
      B.link   ! B.rel "stylesheet" ! B.type_ "text/css"        ! B.href "/static/styles/style.css"
      sequence_ templateSectionHead

    B.body $ do
      B.div ! B.class_ "navbar navbar-fixed-top" $ do
        B.div ! B.class_ "navbar-inner" $ do
          B.div ! B.class_ "container-fluid" $ do
            B.a ! B.class_ "brand" ! B.href "#" $ "Project name"
            B.div ! B.class_ "nav-collapse" $ do
              B.ul ! B.class_ "nav" $ do
                B.li $ B.a ! B.href (route $ I.Core  IC.Home) $ "Home"
                B.li $ B.a ! B.href (route $ I.Forum IF.Home) $ "Forums"

      B.div ! B.class_ "container-fluid" $ do
        B.div ! B.class_ "row-fluid" $ do
          B.div ! B.class_ "span3" $ do
            B.div ! B.class_ "well sidebar-nav" $ do
              B.ul ! B.class_ "nav nav-list" $ do
                B.li $ B.a ! B.href (route $ I.Core  IC.Home) $ icon "home"    <> " Home"
                B.li $ B.a ! B.href (route $ I.Forum IF.Home) $ icon "comment" <> " Forums"

            B.div ! B.class_ "well" $ do
              sequence_ templateSectionL

          B.div ! B.class_ "span6" $ do
            sequence_ templateSectionM

          B.div ! B.class_ "span3" $ do
            B.div ! B.class_ "well" $ do
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
