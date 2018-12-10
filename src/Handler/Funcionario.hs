{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Funcionario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Data.Time
import Data.Aeson
import Data.Aeson.Casing

--POST FUNCIONARIO

--Função que receberá o POST de funcionario
postFuncionarioR :: Handler TypedContent
postFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    funjson <- requireJsonBody :: Handler FunReqJSON
    agora <- liftIO $ getCurrentTime
    funcionario <- return $ createFuncionario agora funjson
    funcionarioid <- runDB $ insert funcionario
    sendStatusJSON created201 (object ["id" .= funcionarioid])
    
--Criando o tipo JSON para receber um novo funcionario
data FunReqJSON = FunReqJSON {
    funreqUsername      :: Text,
    funreqPassword      :: Text,
    funreqNome          :: Text,
    funreqCpf           :: Text,
    funreqRg            :: Text,
    funreqNasc          :: Day,
    funreqCargo         :: Int,
    funreqTelefone      :: Maybe Text,
    funreqCelular       :: Maybe Text,
    funreqEmail         :: Text,
    funreqCep           :: Text,
    funreqEstado        :: Text,
    funreqCidade        :: Text,
    funreqBairro        :: Text,
    funreqLogradouro    :: Text,
    funreqNumero        :: Text,
    funreqComplemento   :: Maybe Text
} deriving (Show, Read, Generic)


--Criando instância de ToJSON e FromJSON
instance ToJSON FunReqJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunReqJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
    
--Função que pega o tempo de agora e o JSON postado para criar o tipo usuario (usado no banco) 
createFuncionario :: UTCTime -> FunReqJSON -> Usuario
createFuncionario agora funjson = 
    Usuario {
        usuarioUsername                = funreqUsername funjson,
        usuarioPassword                = funreqPassword funjson,
        usuarioNome                    = funreqNome funjson,
        usuarioCpf                     = funreqCpf funjson,
        usuarioRg                      = funreqRg funjson,
        usuarioTipo                    = cleanTipo,
        usuarioNasc                    = funreqNasc funjson,
        usuarioTelefone                = funreqTelefone funjson,
        usuarioCelular                 = funreqCelular funjson,
        usuarioEmail                   = funreqEmail funjson,
        usuarioPais                    = "BR",
        usuarioCep                     = funreqCep funjson,
        usuarioEstado                  = funreqEstado funjson,
        usuarioCidade                  = funreqCidade funjson,
        usuarioBairro                  = funreqBairro funjson,
        usuarioLogradouro              = funreqLogradouro funjson,
        usuarioNumero                  = funreqNumero funjson,
        usuarioComplemento             = funreqComplemento funjson,
        usuarioInsertedTimestamp       = agora,
        usuarioLastUpdatedTimestamp    = agora
    }
    where
    cleanTipo = case (funreqCargo funjson) of
        1 -> "Admin"        :: Text
        2 -> "Secretaria"   :: Text
        _ -> "Secretaria"   :: Text
        
        
--GET 1 FUNCIONARIO

--Criando o tipo JSON que mandará o funcioanrio selecionado para o front  
data FunResJSON = FunResJSON {
    funresId            :: UsuarioId,
    funresUsername      :: Text,
    funresNome          :: Text,
    funresCpf           :: Text,
    funresRg            :: Text,
    funresNasc          :: Day,
    funresCargo         :: Int,
    funresTelefone      :: Maybe Text,
    funresCelular       :: Maybe Text,
    funresEmail         :: Text,
    funresCep           :: Text,
    funresEstado        :: Text,
    funresCidade        :: Text,
    funresBairro        :: Text,
    funresLogradouro    :: Text,
    funresNumero        :: Text,
    funresComplemento   :: Maybe Text,
    funresInsertedTimestamp     :: ZonedTime,
    funresLastUpdatedTimestamp  :: ZonedTime
} deriving (Show, Read, Generic)

instance ToJSON FunResJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunResJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
  
  
--Função que receberá o GET e responderá com o JSON do funcionario   
getSingleFuncionarioR :: UsuarioId -> Handler TypedContent
getSingleFuncionarioR usuid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    usuario <- runDB $ get404 usuid
    if (usuarioTipo usuario == "Medico") then
        sendStatusJSON badRequest400 (object ["resp" .= ("Não é funcionário"::Text)])
    else do
        funjson <- return $ createFunGet usuid usuario
        sendStatusJSON ok200 (object ["resp" .= funjson])
    
--Função que recebe um id e o tipo Usuario (do banco) para criar o JSON de resposta 
createFunGet :: UsuarioId -> Usuario -> FunResJSON
createFunGet funcid usu =
    FunResJSON {
        funresId            = funcid,
        funresUsername      = usuarioUsername usu,
        funresNome          = usuarioNome usu,
        funresCpf           = usuarioCpf usu,
        funresRg            = usuarioRg usu,
        funresNasc          = usuarioNasc usu,
        funresCargo         = tipo,
        funresTelefone      = usuarioTelefone usu,
        funresCelular       = usuarioCelular usu,
        funresEmail         = usuarioEmail usu,
        funresCep           = usuarioCep usu,
        funresEstado        = usuarioEstado usu,
        funresCidade        = usuarioCidade usu,
        funresBairro        = usuarioBairro usu,
        funresLogradouro    = usuarioLogradouro usu,
        funresNumero        = usuarioNumero usu,
        funresComplemento   = usuarioComplemento usu,
        funresInsertedTimestamp     = istamp,
        funresLastUpdatedTimestamp  = ustamp
    }
    where
    istamp = utcToZonedTime utc $ usuarioInsertedTimestamp usu
    ustamp = utcToZonedTime utc $ usuarioLastUpdatedTimestamp usu
    tipo = case (usuarioTipo usu) of
        "Admin" -> 1
        "Secretaria" -> 2
        _ -> 2

--GET LIST

--Função que pegará uma "Entidade Usuario" (tipo que vem do banco (id+Usuario)) e criará um JSON de resposta
createFunGetE :: Entity Usuario -> FunResJSON
createFunGetE eFuncionario = createFunGet funcionarioid funcionario
    where
    funcionarioid = entityKey eFuncionario
    funcionario = entityVal eFuncionario

--Função que receberá o GET para a listagem de todos os funcionarios
getListFuncionarioR :: Handler TypedContent
getListFuncionarioR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    eFuncionarios <- runDB $ selectList [UsuarioTipo !=. "Medico"] [Asc UsuarioId]
    funjsons <- return $ map createFunGetE eFuncionarios
    sendStatusJSON ok200 (object ["resp" .= funjsons])
    
--DELETE FUNCIONARIO

--Função que receberá o DELETE com um Id e apaga o paciente do banco de dados
deleteApagarFuncionarioR :: UsuarioId -> Handler TypedContent
deleteApagarFuncionarioR usuid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    usuario <- runDB $ get404 usuid
    if (usuarioTipo usuario == "Medico") then
        sendStatusJSON badRequest400 (object ["resp" .= ("Não é funcionário"::Text)])
    else do
        runDB $ delete usuid
        sendStatusJSON ok200 (object ["resp" .= ("Funcionário deletado"::Text)])

--PUT FUNCIONARIO

--Tipo para receber o JSON de alteração de funcionario, que é diferente do de criação (sem username e senha)
data FunAltJSON = FunAltJSON {
    funaltNome          :: Text,
    funaltCpf           :: Text,
    funaltRg            :: Text,
    funaltNasc          :: Day,
    funaltCargo         :: Int,
    funaltTelefone      :: Maybe Text,
    funaltCelular       :: Maybe Text,
    funaltEmail         :: Text,
    funaltCep           :: Text,
    funaltEstado        :: Text,
    funaltCidade        :: Text,
    funaltBairro        :: Text,
    funaltLogradouro    :: Text,
    funaltNumero        :: Text,
    funaltComplemento   :: Maybe Text
} deriving (Show, Read, Generic)

instance ToJSON FunAltJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON FunAltJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   
--Função que receberá o PUT com um Id e as novas informações do funcionario, colocando no banco com o mesmo id
putAlterarFuncionarioR :: UsuarioId -> Handler TypedContent
putAlterarFuncionarioR usuid = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    funjson <- requireJsonBody :: Handler FunAltJSON
    usuario <- runDB $ get404 usuid
    if (usuarioTipo usuario == "Medico") then
        sendStatusJSON badRequest400 (object ["resp" .= ("Não é funcionário"::Text)])
    else do
        agora <- liftIO $ getCurrentTime
        altFuncionario <- return $ alterFuncionario funjson usuario agora 
        runDB $ replace usuid altFuncionario
        sendStatusJSON ok200 (object ["resp" .= ("Funcionario alterado"::Text)])
   
--Cria um Usuario com as informações novas, porém mantendo a data de criação, o username e a senha antigos.
alterFuncionario :: FunAltJSON -> Usuario -> UTCTime -> Usuario
alterFuncionario funjson usuario agora = 
    Usuario {
        usuarioUsername             = usuarioUsername usuario,
        usuarioPassword             = usuarioPassword usuario,
        usuarioNome                 = funaltNome funjson,
        usuarioCpf                  = funaltCpf funjson,
        usuarioRg                   = funaltRg funjson,
        usuarioTipo                 = cleanTipo,
        usuarioNasc                 = funaltNasc funjson,
        usuarioTelefone             = funaltTelefone funjson,
        usuarioCelular              = funaltCelular funjson,
        usuarioEmail                = funaltEmail funjson,
        usuarioPais                 = "BR",
        usuarioCep                  = funaltCep funjson,
        usuarioEstado               = funaltEstado funjson,
        usuarioCidade               = funaltCidade funjson,
        usuarioBairro               = funaltBairro funjson,
        usuarioLogradouro           = funaltLogradouro funjson,
        usuarioNumero               = funaltNumero funjson,
        usuarioComplemento          = funaltComplemento funjson,
        usuarioInsertedTimestamp    = usuarioInsertedTimestamp usuario,
        usuarioLastUpdatedTimestamp = agora
    }
    where
    cleanTipo = case (funaltCargo funjson) of
        1 -> "Admin"        :: Text
        2 -> "Secretaria"   :: Text
        _ -> "Secretaria"   :: Text