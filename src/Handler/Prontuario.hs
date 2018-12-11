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