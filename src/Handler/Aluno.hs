{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Aluno where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius



formAluno :: Form Aluno
formAluno = renderBootstrap $ Aluno
    <$> areq textField "Nome: " Nothing
    <*> areq intField "Idade: " Nothing
    <*> areq textField "RA: " Nothing

getPerfilR :: AlunoId -> Handler Html
getPerfilR alunoid = do
    aluno <- runDB $ get404 alunoid
    defaultLayout $ do
        $(whamletFile "templates/perfil.hamlet")

getAlunoR :: Handler Html
getAlunoR = do 
    (widgetForm,enctype) <- generateFormPost formAluno
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/formAluno.hamlet")

postAlunoR :: Handler Html
postAlunoR = do
    ((res,_),_) <- runFormPost formAluno
    case res of 
        FormSuccess aluno -> do 
            alunoid <- runDB $ insert aluno
            redirect (PerfilR alunoid)
        _ -> redirect HomeR
        

getListaAlunoR :: Handler Html
getListaAlunoR = do
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            <table class="table">
                <thead>
                    <tr>
                        <th>
                            NOME
                        <th>
                            RA
                        <th>
                            Idade
                <tbody>
                    $forall (Entity alunoid aluno) <- alunos
                        <tr>
                            <td>
                                <a href=@{PerfilR alunoid}>
                                    #{alunoNome aluno}
                            <td>
                                #{alunoRa aluno}
                            <td>
                                #{alunoIdade aluno}
        |]