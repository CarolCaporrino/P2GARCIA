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
import Handler.Login
import Data.Text as T (pack,unpack,Text)
import Text.Read (readMaybe)

--POST


--Criando o tipo JSON para receber um novo medico
data MedReqJSON = MedReqJSON {
    medreqUsername          :: Text,
    medreqPassword          :: Text,
    medreqNome              :: Text,
    medreqCpf               :: Text,
    medreqRg                :: Text,
    medreqCrm               :: Text,
    medreqNasc              :: Maybe Day,
    medreqEspecializacoes   :: [EspecializacaoId],
    medreqTelefone          :: Maybe Text,
    medreqCelular           :: Maybe Text,
    medreqEmail             :: Text,
    medreqCep               :: Maybe Text,
    medreqEstado            :: Maybe Text,
    medreqCidade            :: Maybe Text,
    medreqBairro            :: Maybe Text,
    medreqLogradouro        :: Maybe Text,
    medreqNumero            :: Maybe Text,
    medreqComplemento       :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON MedReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
 
 
optionsMedicoR :: Handler TypedContent
optionsMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])   

--Função que receberá o POST de medico
postMedicoR :: Handler TypedContent
postMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    medjson <- requireJsonBody :: Handler MedReqJSON
    mBearer <- lookupBearerAuth
    execJwt mBearer [1] $ do
        agora <- liftIO $ getCurrentTime
        mUsuario <- return $ createUsuario agora medjson
        case mUsuario of
            Just usuario -> do
                usuid <- runDB $ insert usuario
                medico <- return $ createMedico usuid $ medreqCrm medjson
                medid <- runDB $ insert medico
                especmeds <- return $ createEspecMeds agora medid $ medreqEspecializacoes medjson
                _ <- mapM insEspecMed especmeds
                sendStatusJSON created201 (object ["resp" .= (object ["usuarioid" .= usuid,"medicoid" .= medid])])
            Nothing -> sendStatusJSON badRequest400 (object ["resp" .= ("Inválido"::Text)])
    where
    insEspecMed e = runDB $ insert e :: Handler EspecMedicoId


--Função que pega o tempo de agora e o JSON postado para criar o tipo usuario (usado no banco)    
createUsuario :: UTCTime -> MedReqJSON -> Maybe Usuario
createUsuario agora medjson = do
    senhaHash <- hashPassw $ medreqPassword medjson
    return $ Usuario {
        usuarioUsername     = medreqUsername medjson,
        usuarioPassword     = senhaHash,
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
    medresNasc              :: Maybe Day,
    medresEspecializacoes   :: [EspecJSON],
    medresTelefone          :: Maybe Text,
    medresCelular           :: Maybe Text,
    medresEmail             :: Text,
    medresCep               :: Maybe Text,
    medresEstado            :: Maybe Text,
    medresCidade            :: Maybe Text,
    medresBairro            :: Maybe Text,
    medresLogradouro        :: Maybe Text,
    medresNumero            :: Maybe Text,
    medresComplemento       :: Maybe Text,
    medresAtivo             :: Bool,
    medresInsertedTimestamp     :: ZonedTime,
    medresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON MedResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

optionsSingleMedicoR :: MedicoId -> Handler TypedContent
optionsSingleMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

--Função que receberá o GET e responderá com o JSON do medico
getSingleMedicoR :: MedicoId -> Handler TypedContent
getSingleMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mBearer <- lookupBearerAuth
    execJwt mBearer [1,2,3] $ do
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


--GET LIST

optionsListMedicoR :: Handler TypedContent
optionsListMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    sendStatusJSON ok200 (object [])

getListMedicoR :: Handler TypedContent
getListMedicoR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    mNome <- lookupGetParam $ T.pack "nome"
    mEspec <- lookupGetParam $ T.pack "especializacao"
    mBearer <- lookupBearerAuth
    execJwt mBearer [1,2,3] $ do
        nomeFilter <- return $ createMedFilterNome mNome
        especFilter <- return $ createMedFilterEspec mEspec
        
        eUsuarios <- runDB $ selectList ([UsuarioTipo ==. "Medico"]++nomeFilter) [Asc UsuarioId]
        usuarioids <- return $ map entityKey eUsuarios
        
        eMedicos <- runDB $ selectList [MedicoUserid <-. usuarioids, MedicoAtivo ==. True] [Asc MedicoId]
        medicoids <- return $ map entityKey eMedicos
        
        eEspecmedics <- runDB $ selectList ([EspecMedicoMedicoid <-. medicoids]++especFilter) [Asc EspecMedicoId]
        medicoids' <- return $ map (\eem -> especMedicoMedicoid $ entityVal eem) eEspecmedics
        
        eMedicos' <- runDB $ selectList [MedicoId <-. medicoids'] [Asc MedicoId]
        
        medgetjsons <- mapM createFromMed eMedicos'
        sendStatusJSON ok200 (object ["resp" .= medgetjsons])
    
createMedFilterNome :: Maybe Text -> [Filter Usuario]
createMedFilterNome mNome = 
    case mNome of
        Just nome -> [Filter UsuarioNome (Left $ concat ["%", nome, "%"]) (BackendSpecificFilter "ILIKE")]
        Nothing -> []
        
createMedFilterEspec :: Maybe Text -> [Filter EspecMedico]
createMedFilterEspec mEspec = 
    case mEspec of
        Just espec -> case (readMaybe $ T.unpack espec :: Maybe Int64) of
            Just especint -> [EspecMedicoEspecid ==. (toSqlKey especint)]
            Nothing -> []
        Nothing -> []
    
    
-- ativar / desativar

optionsDesativarMedicoR :: MedicoId -> Handler TypedContent
optionsDesativarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])

patchDesativarMedicoR :: MedicoId -> Handler TypedContent
patchDesativarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    mBearer <- lookupBearerAuth
    execJwt mBearer [1] $ do
        _ <- runDB $ get404 medicoid
        runDB $ update medicoid [MedicoAtivo =. False]
        sendStatusJSON ok200 (object ["resp" .= medicoid])


optionsAtivarMedicoR :: MedicoId -> Handler TypedContent
optionsAtivarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    sendStatusJSON ok200 (object [])
    
patchAtivarMedicoR :: MedicoId -> Handler TypedContent
patchAtivarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION, CONTENT-TYPE"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PATCH"
    mBearer <- lookupBearerAuth
    execJwt mBearer [1] $ do
        _ <- runDB $ get404 medicoid
        runDB $ update medicoid [MedicoAtivo =. True]
        sendStatusJSON ok200 (object ["resp" .= medicoid])
    
    
--PUT

data MedAltJSON = MedAltJSON {
    medaltNome              :: Text,
    medaltCpf               :: Text,
    medaltRg                :: Text,
    medaltCrm               :: Text,
    medaltNasc              :: Maybe Day,
    medaltEspecializacoes   :: [EspecializacaoId],
    medaltTelefone          :: Maybe Text,
    medaltCelular           :: Maybe Text,
    medaltEmail             :: Text,
    medaltCep               :: Maybe Text,
    medaltEstado            :: Maybe Text,
    medaltCidade            :: Maybe Text,
    medaltBairro            :: Maybe Text,
    medaltLogradouro        :: Maybe Text,
    medaltNumero            :: Maybe Text,
    medaltComplemento       :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON MedAltJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MedAltJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
   
optionsAlterarMedicoR :: MedicoId -> Handler TypedContent
optionsAlterarMedicoR _ = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    sendStatusJSON ok200 (object [])   
   
putAlterarMedicoR :: MedicoId -> Handler TypedContent
putAlterarMedicoR medicoid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    addHeader "ACCESS-CONTROL-ALLOW-HEADERS" "AUTHORIZATION"
    addHeader "ACCESS-CONTROL-ALLOW-METHODS" "PUT"
    medico <- runDB $ get404 medicoid
    medjson <- requireJsonBody :: Handler MedAltJSON
    mBearer <- lookupBearerAuth
    execJwt mBearer [1] $ do
        usuid <- return $ medicoUserid medico
        usuario <- runDB $ get404 usuid
        agora <- liftIO $ getCurrentTime
        altUsuario <- return $ createAltUsu agora medjson usuario
        altMedico <- return $ createAltMed usuid medjson medico
        runDB $ deleteWhere [EspecMedicoMedicoid ==. medicoid]
        especmeds <- return $ createEspecMeds agora medicoid $ medaltEspecializacoes medjson
        runDB $ replace usuid altUsuario
        runDB $ replace medicoid altMedico
        _ <- mapM insEspecMed especmeds
        sendStatusJSON ok200 (object ["resp" .= medicoid])
    where
    insEspecMed e = runDB $ insert e :: Handler EspecMedicoId
   

createAltUsu :: UTCTime -> MedAltJSON -> Usuario -> Usuario
createAltUsu agora medjson usu =
    Usuario {
        usuarioUsername     = usuarioUsername usu,
        usuarioPassword     = usuarioPassword usu,
        usuarioNome         = medaltNome medjson,
        usuarioCpf          = medaltCpf medjson,
        usuarioRg           = medaltRg medjson,
        usuarioNasc         = medaltNasc medjson,
        usuarioTipo         = "Medico",
        usuarioTelefone     = medaltTelefone medjson,
        usuarioCelular      = medaltCelular medjson,
        usuarioEmail        = medaltEmail medjson,
        usuarioPais         = "BR",
        usuarioCep          = medaltCep medjson,
        usuarioEstado       = medaltEstado medjson,
        usuarioCidade       = medaltCidade medjson,
        usuarioBairro       = medaltBairro medjson,
        usuarioLogradouro   = medaltLogradouro medjson,
        usuarioNumero       = medaltNumero medjson,
        usuarioComplemento  = medaltComplemento medjson,
        usuarioInsertedTimestamp        = usuarioInsertedTimestamp usu,
        usuarioLastUpdatedTimestamp     = agora
    }
    
    
createAltMed :: UsuarioId -> MedAltJSON -> Medico -> Medico
createAltMed usuarioid medjson med =
    Medico {
        medicoUserid    = usuarioid,
        medicoCrm       = medaltCrm medjson,
        medicoAtivo     = medicoAtivo med
    }