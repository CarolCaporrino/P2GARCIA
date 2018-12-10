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
import Text.Lucius
import Text.Julius
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