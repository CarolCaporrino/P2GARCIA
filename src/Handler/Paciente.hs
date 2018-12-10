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

--POST


--Criando o tipo JSON para receber um novo paciente
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
--Criando instância de ToJSON e FromJSON
instance ToJSON PacReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PacReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


--Função que receberá o POST de paciente
postPacienteR :: Handler TypedContent
postPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    pacjson <- requireJsonBody :: Handler PacReqJSON
    agora <- liftIO $ getCurrentTime
    paciente <- return $ createPaciente agora pacjson
    pacienteid <- runDB $ insert paciente
    sendStatusJSON created201 (object ["id" .= pacienteid])
    

--Função que pega o tempo de agora e o JSON postado para criar o tipo paciente (usado no banco)    
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
    
--GET 1
    
--Criando o tipo JSON que mandará o paciente selecionado para o front    
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
  

--Função que receberá o GET e responderá com o JSON do paciente
getSinglePacienteR :: PacienteId -> Handler TypedContent
getSinglePacienteR pacid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    paciente <- runDB $ get404 pacid
    pacjson <- return $ createPacGet pacid paciente
    sendStatusJSON ok200 (object ["resp" .= pacjson])

--Função que recebe um id e o tipo Paciente (do banco) para criar o JSON de resposta
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


--GET LIST

--Função que pegará uma "Entidade Paciente" (tipo que vem do banco (id+Paciente)) e criará um JSON de resposta
createPacGetE :: Entity Paciente -> PacResJSON
createPacGetE ePaciente = createPacGet pacienteid paciente
    where
    pacienteid = entityKey ePaciente
    paciente = entityVal ePaciente

--Função que receberá o GET para a listagem de todos os pacientes
getListPacienteR :: Handler TypedContent
getListPacienteR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    ePacientes <- runDB $ selectList [] [Asc PacienteId]
    pacjsons <- return $ map createPacGetE ePacientes
    sendStatusJSON ok200 (object ["resp" .= pacjsons])
    
--DELETE

--Função que receberá o DELETE com um Id e apaga o paciente do banco de dados
--(ainda incompleto, pois dependeria do apagar consulta e prontuário também)
deleteApagarPacienteR :: PacienteId -> Handler TypedContent
deleteApagarPacienteR pacid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    _ <- runDB $ get404 pacid
    runDB $ delete pacid
    sendStatusJSON ok200 (object ["resp" .= ("Paciente deletado"::Text)])
