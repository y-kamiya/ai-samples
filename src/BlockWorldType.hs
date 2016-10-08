{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BlockWorldType where

import Language.Haskell.TH

deriveBlock :: Q [Dec]
deriveBlock = return [DataD [] (mkName "Block") [] [NormalC (mkName "A") [], NormalC (mkName "B") []] [''Show, ''Eq, ''Ord, ''Enum]]

