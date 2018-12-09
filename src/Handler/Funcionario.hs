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
