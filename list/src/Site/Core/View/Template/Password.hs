{-# LANGUAGE OverloadedStrings #-}

module Site.Core.View.Template.Password
( templateUpdate
, templateDelete
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Text.Blaze.Html  as B
-- import qualified Text.Blaze.Html5 as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
-- import qualified Site.Core.Model.Type.Profile  as IC
-- import qualified Site.Core.Model.Type.Identity as IC
------------------------------------------------------------------------------

templateUpdate :: B.Markup
               -> DefaultTemplate
templateUpdate form = def
    { templateTitle = "Password Update"
    , templateSectionM = [form]
    }

templateDelete :: B.Markup
               -> DefaultTemplate
templateDelete form = def
    { templateTitle = "Password Deletion"
    , templateSectionM = [form]
    }
