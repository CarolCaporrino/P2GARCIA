{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Consulta where

import Import
import Network.HTTP.Types.Status
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
   
   
--Função que receberá o POST de consulta
postConsultaR :: Handler TypedContent
postConsultaR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
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