{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Bolao where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getBolaoR :: Handler Html
getBolaoR = do
    boloes <- runDB $ selectList [] [Asc BolaoId]
    (bolaoWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "Boloes"
        $(widgetFile "boloes")

postBolaoR :: Handler Html
postBolaoR = do
    ((res,bolaoWidget),enctype) <- runFormPost entryForm
    case res of
        FormSuccess bolao -> do
           --let palpite' = palpite { palpiteUser = userId, palpiteUserName = (userIdent user) }
           bolaoId <- runDB $ insert bolao
           setMessage "Bolão criado"
           redirect BolaoR
        _ -> do
            setMessage "Erro na criacao do bolão"
            redirect BolaoR



createSettigs str = FieldSettings
    { fsLabel = str, fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
    , fsAttrs =
        [ ("class", "form-control")
        ]
    }

entryForm :: Form Bolao
entryForm = renderBootstrap3 BootstrapBasicForm $ Bolao
    <$> areq   textField (createSettigs "Nome") Nothing
    <*> areq   textField (createSettigs "Time da Casa") Nothing
    <*> areq   textField (createSettigs "Time visitante") Nothing