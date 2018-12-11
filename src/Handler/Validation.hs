{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Validation where

import Import
import Data.Char
import Data.List ((!!))
import Text.Read
import Data.ByteString.Char8 as BSC (pack,unpack,ByteString)
import Data.Text as T (pack,unpack,Text)

tipoToInt :: T.Text -> Int
tipoToInt tipo = case tipo of
    "Admin" -> 1
    "Secretaria" -> 2
    "Medico" -> 3
    _ -> 0
    
intToTipo :: Int -> Maybe T.Text
intToTipo tipo = case tipo of
    1 -> Just "Admin"
    2 -> Just "Secretaria"
    3 -> Just "Medico"
    _ -> Nothing
 
    
cargoToInt :: T.Text -> Int
cargoToInt tipo = case tipo of
    "Admin" -> 1
    "Secretaria" -> 2
    _ -> 0

intToCargo :: Int -> Maybe T.Text
intToCargo cargo = case cargo of
    1 -> Just "Admin"
    2 -> Just "Secretaria"
    _ -> Nothing

