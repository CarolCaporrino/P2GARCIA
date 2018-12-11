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