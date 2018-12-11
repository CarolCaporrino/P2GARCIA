{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Medico where

import Import
import Database.Persist.Postgresql
import Data.Time
import Data.Aeson
import Data.Aeson.Casing
import Handler.Especializacao

--POST

--Criando o tipo JSON para receber um novo medico
data MedReqJSON = MedReqJSON {
    medreqUsername          :: Text,
    medreqPassword          :: Text,
    medreqNome              :: Text,
    medreqCpf               :: Text,
    medreqRg                :: Text,
    medreqCrm               :: Text,
    medreqNasc              :: Day,
    medreqEspecializacoes   :: [EspecializacaoId],
    medreqTelefone          :: Maybe Text,
    medreqCelular           :: Maybe Text,
    medreqEmail             :: Text,
    medreqCep               :: Text,
    medreqEstado            :: Text,
    medreqCidade            :: Text,
    medreqBairro            :: Text,
    medreqLogradouro        :: Text,
    medreqNumero            :: Text,
    medreqComplemento       :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON MedReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

--Função que receberá o POST de medico
postMedicoR :: Handler TypedContent
postMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    medjson <- requireJsonBody :: Handler MedReqJSON
    agora <- liftIO $ getCurrentTime
    usuario <- return $ createUsuario agora medjson
    usuid <- runDB $ insert usuario
    medico <- return $ createMedico usuid $ medreqCrm medjson
    medid <- runDB $ insert medico
    especmeds <- return $ createEspecMeds agora medid $ medreqEspecializacoes medjson
    _ <- mapM insEspecMed especmeds
    sendStatusJSON created201 (object ["usuarioid" .= usuid, "medicoid" .= medid])
    where
    insEspecMed e = runDB $ insert e :: Handler EspecMedicoId


--Função que pega o tempo de agora e o JSON postado para criar o tipo usuario (usado no banco)    
createUsuario :: UTCTime -> MedReqJSON -> Usuario
createUsuario agora medjson = do
    Usuario {
        usuarioUsername     = medreqUsername medjson,
        usuarioPassword     = medreqPassword medjson,
        usuarioNome         = medreqNome medjson,
        usuarioCpf          = medreqCpf medjson,
        usuarioRg           = medreqRg medjson,
        usuarioNasc         = medreqNasc medjson,
        usuarioTipo         = "Medico",
        usuarioTelefone     = medreqTelefone medjson,
        usuarioCelular      = medreqCelular medjson,
        usuarioEmail        = medreqEmail medjson,
        usuarioPais         = "BR",
        usuarioCep          = medreqCep medjson,
        usuarioEstado       = medreqEstado medjson,
        usuarioCidade       = medreqCidade medjson,
        usuarioBairro       = medreqBairro medjson,
        usuarioLogradouro   = medreqLogradouro medjson,
        usuarioNumero       = medreqNumero medjson,
        usuarioComplemento  = medreqComplemento medjson,
        usuarioInsertedTimestamp        = agora,
        usuarioLastUpdatedTimestamp     = agora
    }
    
createMedico :: UsuarioId -> Text -> Medico
createMedico usuarioid crm =
    Medico {
        medicoUserid    = usuarioid,
        medicoCrm       = crm,
        medicoAtivo     = True
    }
    
createEspecMeds :: UTCTime -> MedicoId -> [EspecializacaoId] -> [EspecMedico]
createEspecMeds agora medicoid especids = 
    map (\f -> f agora) especsf
    where
    cleanEspecMed especid = EspecMedico medicoid especid
    especsf = map cleanEspecMed especids
    
    
--GET 1
    
--Criando o tipo JSON que mandará o medico selecionado para o front    
data MedResJSON = MedResJSON {
    medresId                :: MedicoId,
    medresUsername          :: Text,
    medresNome              :: Text,
    medresCpf               :: Text,
    medresRg                :: Text,
    medresCrm               :: Text,
    medresNasc              :: Day,
    medresEspecializacoes   :: [EspecJSON],
    medresTelefone          :: Maybe Text,
    medresCelular           :: Maybe Text,
    medresEmail             :: Text,
    medresCep               :: Text,
    medresEstado            :: Text,
    medresCidade            :: Text,
    medresBairro            :: Text,
    medresLogradouro        :: Text,
    medresNumero            :: Text,
    medresComplemento       :: Maybe Text,
    medresAtivo             :: Bool,
    medresInsertedTimestamp     :: ZonedTime,
    medresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON MedResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

--Função que receberá o GET e responderá com o JSON do medico
getSingleMedicoR :: MedicoId -> Handler TypedContent
getSingleMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    medico <- runDB $ get404 medicoid :: Handler Medico
    eMedico <- return $ Entity medicoid medico :: Handler (Entity Medico)
    medgetjson <- createFromMed eMedico
    sendStatusJSON ok200 (object ["resp" .= medgetjson])


createMedGet :: MedicoId -> Usuario -> Medico -> [Entity Especializacao] -> MedResJSON
createMedGet medicoid usuario medico eEspecs = 
    MedResJSON {
        medresId                = medicoid,
        medresUsername          = usuarioUsername usuario,
        medresNome              = usuarioNome usuario,
        medresCpf               = usuarioCpf usuario,
        medresRg                = usuarioRg usuario,
        medresCrm               = medicoCrm medico,
        medresNasc              = usuarioNasc usuario,
        medresEspecializacoes   = map createEspecJSONe eEspecs,
        medresTelefone          = usuarioTelefone usuario,
        medresCelular           = usuarioCelular usuario,
        medresEmail             = usuarioEmail usuario,
        medresCep               = usuarioCep usuario,
        medresEstado            = usuarioEstado usuario,
        medresCidade            = usuarioCidade usuario,
        medresBairro            = usuarioBairro usuario,
        medresLogradouro        = usuarioLogradouro usuario,
        medresNumero            = usuarioNumero usuario,
        medresComplemento       = usuarioComplemento usuario,
        medresAtivo             = medicoAtivo medico,
        medresInsertedTimestamp     = istamp,
        medresLastUpdatedTimestamp  = ustamp
    }
    where
    istamp = utcToZonedTime utc $ usuarioInsertedTimestamp usuario
    ustamp = utcToZonedTime utc $ usuarioLastUpdatedTimestamp usuario
    
    
createFromMed :: Entity Medico -> Handler MedResJSON
createFromMed eMedico = do
    medico <- return $ entityVal eMedico
    medicoid <- return $ entityKey eMedico
    usuarioid <- return $ medicoUserid medico
    usuario <- runDB $ get404 usuarioid
    eEspecmedics <- runDB $ selectList [EspecMedicoMedicoid ==. medicoid] [Asc EspecMedicoId]
    emids <- return $ map (especMedicoEspecid . entityVal) eEspecmedics
    especs <- mapM espec emids
    medgetjson <- return $ createMedGet medicoid usuario medico especs
    return $ medgetjson
    where
    espec emid = Entity (emid) <$> (runDB $ get404 emid) :: Handler (Entity Especializacao)