{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Matricula where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

formMatricula :: Form [DisciplinaId]
formMatricula = renderBootstrap $ areq (multiSelectField discLista) "Lista de disciplias: " Nothing

discLista = do
       entidades <- runDB $ selectList [] [Asc DisciplinaNome] 
       optionsPairs $ fmap (\ent -> (disciplinaNome $ entityVal ent, entityKey ent)) entidades
 
getMatriculaR :: AlunoId -> Handler Html
getMatriculaR alunoid = do 
    aluno <- runDB $ get404 alunoid
    (widgetForm,enctype) <- generateFormPost formMatricula
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/formMatricula.hamlet")

diaHj :: IO Day
diaHj = fmap utctDay getCurrentTime 

postMatriculaR :: AlunoId -> Handler Html
postMatriculaR alunoid = do 
    ((res,_),_) <- runFormPost formMatricula
    case res of 
        FormSuccess discs -> do 
            diaMat <- liftIO diaHj
            _ <- sequence $ map (\did -> runDB $ insert $ Matricula alunoid did diaMat) discs
            redirect (PerfilR alunoid)
        _ -> redirect HomeR

getMatriculadoR :: AlunoId -> Handler Html
getMatriculadoR alunoid = do
    aluno <- runDB $ get404 alunoid
    matriculas <- runDB $ selectList [MatriculaAlunoid ==. alunoid] []
    discids <- return $ fmap (\m -> matriculaDiscid $ entityVal m) matriculas
    disciplinas <- runDB $ selectList [DisciplinaId <-. discids] []
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/matriculado.hamlet")