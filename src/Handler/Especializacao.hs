{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Especializacao where

import Import
import Data.Time
import Data.Aeson
import Data.Aeson.Casing

data EspecsPostJSON = EspecsPostJSON {
    especsEspecializacoes   :: [EspecJSON]
} deriving (Show, Read, Generic)

instance ToJSON EspecsPostJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EspecsPostJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

data EspecJSON = EspecJSON {
    especId         :: EspecializacaoId,
    especNome       :: Text,
    especDescricao  :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON EspecJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EspecJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

--POST ESPEC

postEspecR :: Handler TypedContent
postEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    especsjson <- requireJsonBody :: Handler EspecsPostJSON
    agora <- liftIO $ getCurrentTime
    especs <- return $ createEspecs agora $ especsEspecializacoes especsjson
    especids <- mapM insEspec especs
    sendStatusJSON created201 (object ["resp" .= especids])
    where
    insEspec e = runDB $ insert e :: Handler EspecializacaoId
    
createEspecs :: UTCTime -> [EspecJSON] -> [Especializacao]
createEspecs agora es = 
    map (\f -> f agora) especsf
    where
    cleanEspec e = Especializacao (especNome e)
    especsf = map cleanEspec es