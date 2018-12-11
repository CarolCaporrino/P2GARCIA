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
            bemail <- return $ BS.pack $ T.unpack email
            bsenha <- return $ BS.pack $ novasenha
            res <- NHS.httpLBS
                $ NHS.setRequestBodyJSON (EmailJSON email $ T.pack novasenha)
                $ NHS.parseRequest_ "POST http://brunotcc.dreamhosters.com/wp-json/clinichead/v1/mandaremail1234"
            sendStatusJSON ok200 (decode $ responseBody res :: Maybe Object)