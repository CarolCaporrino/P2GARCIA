{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Prontuario where

import Import
import Database.Persist.Postgresql
import Data.Time
import Data.Aeson
import Data.Aeson.Casing
import Handler.Login



--POST

data ProntReqJSON = ProntReqJSON {
    prontreqPacienteid :: PacienteId,
    prontreqMedicoid   :: Maybe MedicoId,
    prontreqEspecid    :: Maybe EspecializacaoId,
    prontreqConteudo   :: Text
} deriving (Show, Read, Generic)
   
instance ToJSON ProntReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

optionsProntuarioR :: Handler TypedContent
optionsProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

postProntuarioR :: Handler TypedContent
postProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mBearer <- lookupBearerAuth
    prontjson <- requireJsonBody :: Handler ProntReqJSON
    mjwtInfo <- jwtAll mBearer
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x [3] -> do
                    _ <- runDB $ get404 $ prontreqPacienteid prontjson
                    eMedico <- runDB $ getBy404 $ UniqueUserId $ jwjId jwtInfo
                    medid <- return $ entityKey eMedico
                    agora <- liftIO $ getCurrentTime
                    prontuario <- return $ createPront agora medid prontjson
                    prontid <- runDB $ insert prontuario
                    sendStatusJSON created201 (object ["resp" .= prontid])
                _ -> sendStatusJSON forbidden403 (object ["resp" .= ("Acesso Negado"::Text)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= ("Não Autenticado"::Text)])
       
        
createPront :: UTCTime -> MedicoId -> ProntReqJSON -> EntradaProntuario
createPront agora medid prontuario = do
    EntradaProntuario {
        entradaProntuarioPacienteid     = prontreqPacienteid prontuario,
        entradaProntuarioMedicoid       = medid,
        --entradaProntuarioEspecid        = prontreqEspecid prontuario,
        entradaProntuarioEspecid        = Nothing,
        entradaProntuarioConteudo       = prontreqConteudo prontuario,
        entradaProntuarioTimestamp      = agora
    }
    
    
-- GET 1

data ProntResJSON = ProntResJSON {
    prontresId          :: EntradaProntuarioId,
    prontresPacienteid  :: PacienteId,
    prontresMedicoid    :: MedicoId,
    prontresEspecid     :: Maybe EspecializacaoId,
    prontresConteudo    :: Text,
    prontresTimestamp   :: ZonedTime
} deriving (Show, Read, Generic)
   
instance ToJSON ProntResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON ProntResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


optionsSingleProntuarioR :: EntradaProntuarioId -> Handler TypedContent
optionsSingleProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getSingleProntuarioR :: EntradaProntuarioId -> Handler TypedContent
getSingleProntuarioR prontuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    prontuario <- runDB $ get404 prontuarioid
    prontjson <- return $ createProntGet prontuarioid prontuario
    sendStatusJSON ok200 (object ["resp" .= prontjson])
    
    
createProntGet :: EntradaProntuarioId -> EntradaProntuario -> ProntResJSON
createProntGet prontuarioid prontuario = 
    ProntResJSON {
        prontresId          = prontuarioid,
        prontresPacienteid  = entradaProntuarioPacienteid prontuario,
        prontresMedicoid    = entradaProntuarioMedicoid prontuario,
        prontresEspecid     = entradaProntuarioEspecid prontuario,
        prontresConteudo    = entradaProntuarioConteudo prontuario,
        prontresTimestamp   = utcToZonedTime utc $ entradaProntuarioTimestamp prontuario
    }
    
    
--GET LIST

optionsListProntuarioR :: Handler TypedContent
optionsListProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListProntuarioR :: Handler TypedContent
getListProntuarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    eProntuarios <- runDB $ selectList [] [Asc EntradaProntuarioId]
    prontjsons <- return $ map createProntGetE eProntuarios
    sendStatusJSON created201 (object ["resp" .= prontjsons])
    
createProntGetE :: Entity EntradaProntuario -> ProntResJSON
createProntGetE eProntuario = createProntGet prontuarioid prontuario
    where
    prontuarioid = entityKey eProntuario
    prontuario = entityVal eProntuario
    
--GET PAC

optionsPacProntuarioR :: PacienteId -> Handler TypedContent
optionsPacProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getPacProntuarioR :: PacienteId -> Handler TypedContent
getPacProntuarioR pacienteid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    eProntuarios <- runDB $ selectList [EntradaProntuarioPacienteid ==. pacienteid] [Asc EntradaProntuarioId]
    prontjsons <- return $ map createProntGetE eProntuarios
    sendStatusJSON created201 (object ["resp" .= prontjsons])
  
    
--DELETE

optionsApagarProntuarioR :: EntradaProntuarioId -> Handler TypedContent
optionsApagarProntuarioR  _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    sendStatusJSON ok200 (object [])

deleteApagarProntuarioR :: EntradaProntuarioId -> Handler TypedContent
deleteApagarProntuarioR prontuarioid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "DELETE"
    _ <- runDB $ get404 prontuarioid
    runDB $ delete prontuarioid
    sendStatusJSON ok200 (object ["resp" .= ("Prontuário deletado"::Text)])