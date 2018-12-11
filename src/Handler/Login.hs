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
import Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Data.ByteString as BS
import Data.Text as T (pack,unpack,Text)
import Data.Time.Clock.POSIX
import Jose.Jws
import Jose.Jwa
import Jose.Jwt


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