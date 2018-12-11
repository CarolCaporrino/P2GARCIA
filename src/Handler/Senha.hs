{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Senha where

import Import
import Data.Time
import Data.ByteString.Char8 as BS (pack, unpack)
import Data.ByteString.Lazy as BSL (toStrict)
import Data.Text as T (pack,unpack,Text)
import Handler.Login
import Test.RandomStrings
import Data.Aeson
import Data.Aeson.Casing
import Network.HTTP.Simple as NHS


data EmailJSON = EmailJSON {
    emailEmail :: Text,
    emailSenha :: Text
} deriving Generic

instance ToJSON EmailJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON EmailJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
   
optionsRedefinirSenhaR :: Text -> Handler TypedContent
optionsRedefinirSenhaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchRedefinirSenhaR :: Text -> Handler TypedContent
patchRedefinirSenhaR username = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    eUsuario <- runDB $ getBy404 $ UniqueUserUsername username
    usuarioid <- return $ entityKey eUsuario
    email <- return $ usuarioEmail $ entityVal eUsuario
    novasenha <- liftIO $ randomString (onlyAlphaNum randomASCII) 10
    mHsenha <- return $ hashPassw $ T.pack novasenha
    case mHsenha of
        Nothing -> sendStatusJSON badRequest400 (object [])
        Just hsenha -> do
            runDB $ update usuarioid [UsuarioPassword =. hsenha]
            res <- NHS.httpLBS
                $ NHS.setRequestBodyJSON (EmailJSON email $ T.pack novasenha)
                $ NHS.parseRequest_ "POST http://brunotcc.dreamhosters.com/wp-json/clinichead/v1/mandaremail1234"
            sendStatusJSON ok200 (decode $ responseBody res :: Maybe Object)
 
--Alterar senha 
            
data AltSenhaJSON = AltSenhaJSON {
    senhaPassword   :: Text,
    senhaPassword2  :: Text
} deriving Generic

instance ToJSON AltSenhaJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON AltSenhaJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


optionsAlterarSenhaR :: Handler TypedContent
optionsAlterarSenhaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchAlterarSenhaR :: Handler TypedContent
patchAlterarSenhaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    mBearer <- lookupBearerAuth
    senhajson <- requireJsonBody :: Handler AltSenhaJSON
    mjwtInfo <- jwtAll mBearer
    case mjwtInfo of
        Just jwtInfo -> do
            isValid <- return $ (senhaPassword senhajson == senhaPassword2 senhajson)
            usuid <- return $ jwjId jwtInfo
            if (not isValid) then
                sendStatusJSON badRequest400 (object ["resp" .= ("Invalido"::Text)])
            else do
                _ <- runDB $ get404 usuid
                mHsenha <- return $ hashPassw $ senhaPassword senhajson
                case mHsenha of
                    Nothing -> sendStatusJSON badRequest400 (object ["resp" .= ("Invalido"::Text)])
                    Just hsenha -> do
                        runDB $ update usuid [UsuarioPassword =. hsenha]
                        sendStatusJSON ok200 (object ["resp" .= usuid])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= ("Não autenticado"::Text)])