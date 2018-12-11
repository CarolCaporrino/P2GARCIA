{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql
import Data.Time
import Data.Aeson
import Data.Aeson.Casing
import Crypto.BCrypt
import Data.ByteString.Char8 as BSC (pack,unpack,ByteString)
import Data.Text as T (pack,unpack,Text)


hashPassw :: T.Text -> Maybe T.Text
hashPassw pass = do 
    bsPass <- hashPassword (BSC.pack $ T.unpack pass) (BSC.pack "$2y$04$kqhhCdZNU8A3CW0s4zPKGeqRSAAk9Oj/C/sK.U9YxhLak258QNfIK")
    return $ T.pack $ BSC.unpack bsPass