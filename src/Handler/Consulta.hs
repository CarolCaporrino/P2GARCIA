{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Consulta where

import Import
import Database.Persist.Postgresql
import Data.Time
import Data.Aeson
import Data.Aeson.Casing



--POST


--Criando o tipo JSON para receber uma nova consulta
data ConsReqJSON = ConsReqJSON {
    consreqPacienteid  :: PacienteId,
    consreqMedicoid    :: MedicoId,
    consreqEspecid     :: EspecializacaoId,
    consreqInicio      :: ZonedTime,
    consreqTermino     :: ZonedTime,
    consreqObservacoes :: Text
} deriving (Show, Read, Generic)
--Criando instância de ToJSON e FromJSON
instance ToJSON ConsReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
 
 
optionsConsultaR :: Handler TypedContent
optionsConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])  
   
--Função que receberá o POST de consulta
postConsultaR :: Handler TypedContent
postConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    consjson <- requireJsonBody :: Handler ConsReqJSON
    _ <- runDB $ get404 $ consreqPacienteid consjson
    _ <- runDB $ get404 $ consreqMedicoid consjson
    _ <- runDB $ get404 $ consreqEspecid consjson
    agora <- liftIO $ getCurrentTime
    consulta <- return $ createConsulta agora consjson
    consultaid <- runDB $ insert consulta
    sendStatusJSON created201 (object ["id" .= consultaid])
    

--Função que pega o tempo de agora e o JSON postado para criar o tipo Consulta (usado no banco)    
createConsulta :: UTCTime -> ConsReqJSON -> Consulta
createConsulta agora consjson = 
    Consulta {
        consultaPacienteid      = consreqPacienteid consjson,
        consultaMedicoid        = consreqMedicoid consjson,
        consultaEspecid         = consreqEspecid consjson,
        consultaInicio          = zonedTimeToUTC $ consreqInicio consjson,
        consultaTermino         = zonedTimeToUTC $ consreqTermino consjson,
        consultaObservacoes     = consreqObservacoes consjson,
        consultaInsertedTimestamp       = agora,
        consultaLastUpdatedTimestamp    = agora
    }
    

--GET 1
    
--Criando o tipo JSON que mandará a consulta selecionada para o front    
data ConsResJSON = ConsResJSON {
    consresId           :: ConsultaId,
    consresPacienteid   :: PacienteId,
    consresMedicoid     :: MedicoId,
    consresEspecid      :: EspecializacaoId,
    consresInicio       :: ZonedTime,
    consresTermino      :: ZonedTime,
    consresObservacoes  :: Text,
    consresInsertedTimestamp    :: ZonedTime,
    consresLastUpdatedTimestamp :: ZonedTime
} deriving (Show, Read, Generic)
   
instance ToJSON ConsResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ConsResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase  
   
optionsSingleConsultaR :: ConsultaId -> Handler TypedContent
optionsSingleConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])
   
--Função que receberá o GET e responderá com o JSON da consulta
getSingleConsultaR :: ConsultaId -> Handler TypedContent
getSingleConsultaR consid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    consulta <- runDB $ get404 consid
    consjson <- return $ createConsGet consid consulta
    sendStatusJSON ok200 (object ["resp" .= consjson])

--Função que recebe um id e o tipo Consulta (do banco) para criar o JSON de resposta
createConsGet :: ConsultaId -> Consulta -> ConsResJSON
createConsGet consultaid consulta = 
    ConsResJSON {
        consresId           = consultaid,
        consresPacienteid   = consultaPacienteid consulta,
        consresMedicoid     = consultaMedicoid consulta,
        consresEspecid      = consultaEspecid consulta,
        consresInicio       = utcToZonedTime utc $ consultaInicio consulta,
        consresTermino      = utcToZonedTime utc $ consultaTermino consulta,
        consresObservacoes  = consultaObservacoes consulta,
        consresInsertedTimestamp    = utcToZonedTime utc $ consultaInsertedTimestamp consulta,
        consresLastUpdatedTimestamp = utcToZonedTime utc $ consultaLastUpdatedTimestamp consulta
    }
    
    
--GET LIST

--Função que pegará uma "Entidade Consulta" (tipo que vem do banco (id+Consulta)) e criará um JSON de resposta
createConsGetE :: Entity Consulta -> ConsResJSON
createConsGetE eConsulta = createConsGet consultaid consulta
    where
    consultaid = entityKey eConsulta
    consulta = entityVal eConsulta


optionsListConsultaR :: Handler TypedContent
optionsListConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

--Função que receberá o GET para a listagem de todos os pacientes
getListConsultaR :: Handler TypedContent
getListConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    eConsultas <- runDB $ selectList [] [Asc ConsultaId]
    consjsons <- return $ map createConsGetE eConsultas
    sendStatusJSON ok200 (object ["resp" .= consjsons])
    

--GET LIST POR MEDICO

optionsMedConsultaR :: MedicoId -> Handler TypedContent
optionsMedConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

--Função que recebe um id de médico e retorna todas suas consultas
getMedConsultaR :: MedicoId -> Handler TypedContent
getMedConsultaR medid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    eConsultas <- runDB $ selectList [ConsultaMedicoid ==. medid] [Asc ConsultaId]
    consjsons <- return $ map createConsGetE eConsultas
    sendStatusJSON ok200 (object ["resp" .= consjsons])
    
    
--DELETE


optionsApagarConsultaR :: ConsultaId -> Handler TypedContent
optionsApagarConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    sendStatusJSON ok200 (object [])   
    
--Função que receberá o DELETE com um Id e apaga a consulta do banco de dados
deleteApagarConsultaR :: ConsultaId -> Handler TypedContent
deleteApagarConsultaR consid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    _ <- runDB $ get404 consid
    runDB $ delete consid
    sendStatusJSON ok200 (object ["resp" .= ("Consulta deletada"::Text)])



--PUT

optionsAlterarConsultaR :: ConsultaId -> Handler TypedContent
optionsAlterarConsultaR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])


--Função que receberá o PUT com um Id e as novas informações da consulta, colocando no banco com o mesmo id
putAlterarConsultaR :: ConsultaId -> Handler TypedContent
putAlterarConsultaR consid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    consulta <- runDB $ get404 consid
    consjson <- requireJsonBody :: Handler ConsReqJSON
    agora <- liftIO $ getCurrentTime
    altConsulta <- return $ alterConsulta consjson consulta agora
    runDB $ replace consid altConsulta
    sendStatusJSON ok200 (object ["resp" .= ("Consulta alterada"::Text)])
    

--Cria uma Consulta com as informações novas, porém mantendo a data de criação antiga.
alterConsulta :: ConsReqJSON -> Consulta -> UTCTime -> Consulta
alterConsulta consjson consulta agora = do
    Consulta {
        consultaPacienteid      = consreqPacienteid consjson,
        consultaMedicoid        = consreqMedicoid consjson,
        consultaEspecid         = consreqEspecid consjson,
        consultaInicio          = zonedTimeToUTC $ consreqInicio consjson,
        consultaTermino         = zonedTimeToUTC $ consreqTermino consjson,
        consultaObservacoes     = consreqObservacoes consjson,
        consultaInsertedTimestamp       = consultaInsertedTimestamp consulta,
        consultaLastUpdatedTimestamp    = agora
    }