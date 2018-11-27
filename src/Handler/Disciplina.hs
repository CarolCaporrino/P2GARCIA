{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Disciplina where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius



formDisc :: Form Disciplina
formDisc = renderBootstrap $ Disciplina
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Codigo: " Nothing
    <*> areq intField "Cargo: " Nothing



getDisciplinaR :: Handler Html
getDisciplinaR = do 
    (widgetForm,enctype) <- generateFormPost formDisc
    mensa <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/formDisc.hamlet")

postDisciplinaR :: Handler Html
postDisciplinaR = do
    ((res,_),_) <- runFormPost formDisc
    case res of 
        FormSuccess disc -> do 
            did <- runDB $ insert disc
            setMessage [shamlet|
                Disciplina inserida
            |]
            redirect (DisciplinaR)
        _ -> redirect HomeR