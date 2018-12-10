{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Funcionario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Data.Time
import Data.Aeson
import Data.Aeson.Casing

--POST FUNCIONARIO
postFuncionarioR :: Handler TypedContent
postFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    funjson <- requireJsonBody :: Handler FunReqJSON
    agora <- liftIO $ getCurrentTime
    funcionario <- return $ createFuncionario agora funjson
    funcionarioid <- runDB $ insert funcionario
    sendStatusJSON created201 (object ["id" .= funcionarioid])
    
data FunReqJSON = FunReqJSON {
    funreqUsername      :: Text,
    funreqPassword      :: Text,
    funreqNome          :: Text,
    funreqCpf           :: Text,
    funreqRg            :: Text,
    funreqNasc          :: Day,
    funreqCargo         :: Int,
    funreqTelefone      :: Maybe Text,
    funreqCelular       :: Maybe Text,
    funreqEmail         :: Text,
    funreqCep           :: Text,
    funreqEstado        :: Text,
    funreqCidade        :: Text,
    funreqBairro        :: Text,
    funreqLogradouro    :: Text,
    funreqNumero        :: Text,
    funreqComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON FunReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
    
createFuncionario :: UTCTime -> FunReqJSON -> Usuario
createFuncionario agora funjson = 
    Usuario {
        usuarioUsername                = funreqUsername funjson,
        usuarioPassword                = funreqPassword funjson,
        usuarioNome                    = funreqNome funjson,
        usuarioCpf                     = funreqCpf funjson,
        usuarioRg                      = funreqRg funjson,
        usuarioTipo                    = cleanTipo,
        usuarioNasc                    = funreqNasc funjson,
        usuarioTelefone                = funreqTelefone funjson,
        usuarioCelular                 = funreqCelular funjson,
        usuarioEmail                   = funreqEmail funjson,
        usuarioPais                    = "BR",
        usuarioCep                     = funreqCep funjson,
        usuarioEstado                  = funreqEstado funjson,
        usuarioCidade                  = funreqCidade funjson,
        usuarioBairro                  = funreqBairro funjson,
        usuarioLogradouro              = funreqLogradouro funjson,
        usuarioNumero                  = funreqNumero funjson,
        usuarioComplemento             = funreqComplemento funjson,
        usuarioInsertedTimestamp       = agora,
        usuarioLastUpdatedTimestamp    = agora
    }
    where
    cleanTipo = case (funreqCargo funjson) of
        1 -> "Admin"        :: Text
        2 -> "Secretaria"   :: Text
        _ -> "Secretaria"   :: Text
        
data FunResJSON = FunResJSON {
    funresId            :: UsuarioId,
    funresUsername      :: Text,
    funresNome          :: Text,
    funresCpf           :: Text,
    funresRg            :: Text,
    funresNasc          :: Day,
    funresCargo         :: Int,
    funresTelefone      :: Maybe Text,
    funresCelular       :: Maybe Text,
    funresEmail         :: Text,
    funresCep           :: Text,
    funresEstado        :: Text,
    funresCidade        :: Text,
    funresBairro        :: Text,
    funresLogradouro    :: Text,
    funresNumero        :: Text,
    funresComplemento   :: Maybe Text,
    funresInsertedTimestamp     :: ZonedTime,
    funresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON FunResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
  
   
getSingleFuncionarioR :: UsuarioId -> Handler TypedContent
getSingleFuncionarioR funid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    funcionario <- runDB $ get404 funid
    funjson <- return $ createFunGet funid funcionario
    sendStatusJSON ok200 (object ["resp" .= funjson])
    
createFunGet :: UsuarioId -> Usuario -> FunResJSON
createFunGet funcid usu =
    FunResJSON {
        funresId            = funcid,
        funresUsername      = usuarioUsername usu,
        funresNome          = usuarioNome usu,
        funresCpf           = usuarioCpf usu,
        funresRg            = usuarioRg usu,
        funresNasc          = usuarioNasc usu,
        funresCargo         = tipo,
        funresTelefone      = usuarioTelefone usu,
        funresCelular       = usuarioCelular usu,
        funresEmail         = usuarioEmail usu,
        funresCep           = usuarioCep usu,
        funresEstado        = usuarioEstado usu,
        funresCidade        = usuarioCidade usu,
        funresBairro        = usuarioBairro usu,
        funresLogradouro    = usuarioLogradouro usu,
        funresNumero        = usuarioNumero usu,
        funresComplemento   = usuarioComplemento usu,
        funresInsertedTimestamp     = istamp,
        funresLastUpdatedTimestamp  = ustamp
    }
    where
    istamp = utcToZonedTime utc $ usuarioInsertedTimestamp usu
    ustamp = utcToZonedTime utc $ usuarioLastUpdatedTimestamp usu
    tipo = case (usuarioTipo usu) of
        "Admin" -> 1
        "Secretaria" -> 2
        _ -> 2
