{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)


instance Yesod App where
    makeLogger = return . appLogger
    authRoute _ = Just LoginR
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized AdminR _ = ehAdmin
    isAuthorized (ApagarR _) _ = ehAdmin
    isAuthorized _ _ = ehUsuario
    

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

ehUsuario = do
    logado <- lookupSession "_USR"
    case logado of
        Just _ -> return $ Authorized
        Nothing -> return AuthenticationRequired
        
ehAdmin = do
    logado <- lookupSession "_USR"
    case logado of
        Just "admin" -> return $ Authorized
        Just _ -> return $ Unauthorized "Vc nao eh admin!"
        Nothing -> return AuthenticationRequired
        
        