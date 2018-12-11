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
    
    
--GET ESPEC

optionsSingleEspecializacaoR :: EspecializacaoId -> Handler TypedContent
optionsSingleEspecializacaoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleEspecializacaoR :: EspecializacaoId -> Handler TypedContent
getSingleEspecializacaoR especid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    espec <- runDB $ get404 especid
    especjson <- return $ createEspecJSON especid espec
    sendStatusJSON ok200 (object ["resp" .= especjson])
    
createEspecJSON :: EspecializacaoId -> Especializacao -> EspecJSON
createEspecJSON especid espec =
    EspecJSON especid (especializacaoNome espec) Nothing


optionsListEspecR :: Handler TypedContent
optionsListEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListEspecR :: Handler TypedContent
getListEspecR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    especs <- runDB $ selectList [] [Asc EspecializacaoId]
    especsjson <- return $ map createEspecJSONe especs
    sendStatusJSON ok200 (object ["resp" .= especsjson])
    
createEspecJSONe :: Entity Especializacao -> EspecJSON
createEspecJSONe e = createEspecJSON especid espec
    where
    especid = entityKey e
    espec = entityVal e