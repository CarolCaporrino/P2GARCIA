{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Paciente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Data.Time
import Data.Aeson
import Data.Aeson.Casing


data PacReqJSON = PacReqJSON {
    pacreqNome          :: Text,
    pacreqCpf           :: Text,
    pacreqRg            :: Text,
    pacreqNasc          :: Day,
    pacreqTelefone      :: Maybe Text,
    pacreqCelular       :: Maybe Text,
    pacreqEmail         :: Text,
    pacreqCep           :: Text,
    pacreqEstado        :: Text,
    pacreqCidade        :: Text,
    pacreqBairro        :: Text,
    pacreqLogradouro    :: Text,
    pacreqNumero        :: Text,
    pacreqComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON PacReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


postPacienteR :: Handler TypedContent
postPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    pacjson <- requireJsonBody :: Handler PacReqJSON
    agora <- liftIO $ getCurrentTime
    paciente <- return $ createPaciente agora pacjson
    pacienteid <- runDB $ insert paciente
    sendStatusJSON created201 (object ["id" .= pacienteid])
    
    
createPaciente :: UTCTime -> PacReqJSON -> Paciente
createPaciente agora pacjson = 
    Paciente {
        pacienteNome                    = pacreqNome pacjson,
        pacienteCpf                     = pacreqCpf pacjson,
        pacienteRg                      = pacreqRg pacjson,
        pacienteNasc                    = pacreqNasc pacjson,
        pacienteTelefone                = pacreqTelefone pacjson,
        pacienteCelular                 = pacreqCelular pacjson,
        pacienteEmail                   = pacreqEmail pacjson,
        pacientePais                    = "BR",
        pacienteCep                     = pacreqCep pacjson,
        pacienteEstado                  = pacreqEstado pacjson,
        pacienteCidade                  = pacreqCidade pacjson,
        pacienteBairro                  = pacreqBairro pacjson,
        pacienteLogradouro              = pacreqLogradouro pacjson,
        pacienteNumero                  = pacreqNumero pacjson,
        pacienteComplemento             = pacreqComplemento pacjson,
        pacienteInsertedTimestamp       = agora,
        pacienteLastUpdatedTimestamp    = agora
    }
    
data PacResJSON = PacResJSON {
    pacresId            :: PacienteId,
    pacresNome          :: Text,
    pacresCpf           :: Text,
    pacresRg            :: Text,
    pacresNasc          :: Day,
    pacresTelefone      :: Maybe Text,
    pacresCelular       :: Maybe Text,
    pacresEmail         :: Text,
    pacresCep           :: Text,
    pacresEstado        :: Text,
    pacresCidade        :: Text,
    pacresBairro        :: Text,
    pacresLogradouro    :: Text,
    pacresNumero        :: Text,
    pacresComplemento   :: Maybe Text,
    pacresInsertedTimestamp     :: ZonedTime,
    pacresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON PacResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
getSinglePacienteR :: PacienteId -> Handler TypedContent
getSinglePacienteR pacid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    paciente <- runDB $ get404 pacid
    pacjson <- return $ createPacGet pacid paciente
    sendStatusJSON ok200 (object ["resp" .= pacjson])
    
createPacGet :: PacienteId -> Paciente -> PacResJSON
createPacGet pacienteid pac =
    PacResJSON {
        pacresId            = pacienteid,
        pacresNome          = pacienteNome pac,
        pacresCpf           = pacienteCpf pac,
        pacresRg            = pacienteRg pac,
        pacresNasc          = pacienteNasc pac,
        pacresTelefone      = pacienteTelefone pac,
        pacresCelular       = pacienteCelular pac,
        pacresEmail         = pacienteEmail pac,
        pacresCep           = pacienteCep pac,
        pacresEstado        = pacienteEstado pac,
        pacresCidade        = pacienteCidade pac,
        pacresBairro        = pacienteBairro pac,
        pacresLogradouro    = pacienteLogradouro pac,
        pacresNumero        = pacienteNumero pac,
        pacresComplemento   = pacienteComplemento pac,
        pacresInsertedTimestamp     = istamp,
        pacresLastUpdatedTimestamp  = ustamp
    }
    where
    istamp = utcToZonedTime utc $ pacienteInsertedTimestamp pac
    ustamp = utcToZonedTime utc $ pacienteLastUpdatedTimestamp pac