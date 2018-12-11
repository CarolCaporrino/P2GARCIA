{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql
import Data.Time
import Data.Aeson
import Data.Aeson.Casing
import Crypto.BCrypt
import Data.ByteString.Lazy.Char8   as BSLC (pack,unpack)
import Data.ByteString.Lazy         as BSL  (fromStrict,toStrict)
import Data.ByteString.Char8        as BSC  (pack,unpack)
import Data.ByteString              as BS   (pack,unpack,ByteString)
import Data.Text                    as T    (pack,unpack,Text)
import Data.Time.Clock.POSIX
import Jose.Jws
import Jose.Jwa
import Jose.Jwt


execJwt :: (Maybe T.Text) -> [Int] -> Handler TypedContent -> Handler TypedContent
execJwt mjwt perm func = do
    mjwtInfo <- jwtAll mjwt
    case mjwtInfo of
        Just jwtInfo -> do
            case (jwjCargo jwtInfo) of
                x | elem x perm -> func
                _ -> sendStatusJSON forbidden403 (object ["resp" .= ("Não autorizado"::Text)])
        Nothing -> sendStatusJSON unauthorized401 (object ["resp" .= ("Não autenticado"::Text)])


jwtAll :: (Maybe T.Text) -> Handler (Maybe JwtJSON)
jwtAll mjwt = do
    case mjwt of
        Just jwt -> do
            eitherJwt <- return $ hmacDecode jwtKey $ BSC.pack $ T.unpack jwt
            case eitherJwt of
                Right jwtr -> do
                    maybeJwtinfo <- return $ Data.Aeson.decode $ BSL.fromStrict $ snd jwtr :: Handler (Maybe JwtJSON)
                    case maybeJwtinfo of
                        Just jwtinfo -> do
                            expdate <- return $ jwjExp jwtinfo
                            validoData <- liftIO $ checkDate expdate
                            validoUsu <- checkUsuario $ jwjId jwtinfo
                            --valido <- return $ True
                            if (validoData && validoUsu) then 
                                return $ Just $ jwtinfo
                            else
                                return $ Nothing
                        Nothing -> return $ Nothing
                Left _ -> return $ Nothing
        Nothing -> return $ Nothing
        
checkUsuario :: UsuarioId -> Handler Bool
checkUsuario usuid = do
    musu <- runDB $ get usuid
    case musu of
        Just _ -> return $ True
        Nothing -> return $ False

checkDate :: POSIXTime -> IO Bool
checkDate expdate = do
    expire <- return $ posixSecondsToUTCTime expdate
    now <- getCurrentTime
    return (expire >= now)


hashPassw :: T.Text -> Maybe T.Text
hashPassw pass = do 
    bsPass <- hashPassword (BSC.pack $ T.unpack pass) (BSC.pack "$2y$04$kqhhCdZNU8A3CW0s4zPKGeqRSAAk9Oj/C/sK.U9YxhLak258QNfIK")
    return $ T.pack $ BSC.unpack bsPass
    
data LoginPost = LoginPost {
    loginpostUsername :: Text,
    loginpostPassword :: Text
} deriving Generic

instance ToJSON LoginPost where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON LoginPost where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
   

data JwtJSON = JwtJSON {
    jwjExp      :: POSIXTime,
    jwjId       :: UsuarioId,
    jwjNome     :: Text,
    jwjUsername :: Text,
    jwjCargo    :: Int
} deriving Generic

instance ToJSON JwtJSON where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON JwtJSON where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase



makeJwtClaims :: UsuarioId -> Text -> Text -> Int -> IO JwtJSON
makeJwtClaims usuarioid nome username tipo = do
  currentUTC <- getCurrentTime
  let laterDate = utcTimeToPOSIXSeconds $ addUTCTime (60 * 60 * 16) currentUTC
  return $
    JwtJSON {
        jwjExp      = laterDate,
        jwjId       = usuarioid,
        jwjNome     = nome,
        jwjUsername = username,
        jwjCargo    = tipo
    }

            
jwtKey :: BS.ByteString
jwtKey = "wkjfhfkuheuihgKUGYKJGYrekjygKJefgkG54W5E1FGKAUHWEFKJBfkjWBEJEFS35EWG458415"

   

postLoginR :: Handler TypedContent
postLoginR = do
    addHeader "ACCESS-CONTROL-ALLOW-ORIGIN" "*"
    loginjson <- requireJsonBody :: Handler LoginPost
    lusrname <- return $ loginpostUsername loginjson
    lpssword <- return $ loginpostPassword loginjson
    mEusuario <- runDB $ getBy $ UniqueUserUsername lusrname
    case mEusuario of
        Just eusuario -> do
            mEmedico <- runDB $ getBy $ UniqueUserId $ entityKey eusuario
            mAtivo <- return $ medicoAtivo <$> (entityVal <$> mEmedico)
            isAtivo <- return $ maybeBool mAtivo
            bpssword <- return $ BSC.pack $ T.unpack lpssword
            hpssword <- return $ BSC.pack $ T.unpack $ usuarioPassword $ entityVal eusuario
            if (validatePassword hpssword bpssword && isAtivo) then do
                tipo <- return $ tipoInt $ usuarioTipo $ entityVal eusuario
                username <- return $ usuarioUsername $ entityVal eusuario
                usuarioid <- return $ entityKey eusuario
                nome <- return $ usuarioNome $ entityVal eusuario
                info <- liftIO $ makeJwtClaims usuarioid nome username tipo
                infojson <- return $ BSL.toStrict $ Data.Aeson.encode info
                eitherJwt <- return $ hmacEncode HS384 jwtKey infojson
                case eitherJwt of
                    Right jwt -> do
                        jwtT <- return $ unJwt jwt
                        jwtS <- return $ BSC.unpack jwtT
                        sendStatusJSON ok200 (object ["jwt" .= jwtS])
                    Left _ -> do
                        sendStatusJSON unauthorized401 (object [])
            else
                sendStatusJSON unauthorized401 (object ["resp" .= T.pack "Senha Inválida"])
        Nothing -> do
            sendStatusJSON unauthorized401 (object ["resp" .= T.pack "Usuário Inválido"])
    where
    tipoInt tp = case tp of
        "Admin"         -> 1
        "Secretaria"    -> 2
        "Medico"        -> 3
    maybeBool (Just True) = True
    maybeBool (Just False) = False
    maybeBool Nothing = True