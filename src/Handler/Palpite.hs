{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Palpite where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

--import Yesod.Form.Nic (YesodNic, nicHtmlField)
--instance YesodNic App

getPalpiteR :: Handler Html
getPalpiteR = do
    palpites <- runDB $ selectList [] [Asc PalpiteId]
    (palpiteWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "Palpites"
        $(widgetFile "palpites")


postPalpiteR :: Handler Html
postPalpiteR = do
    maybeCurrentUserId <- maybeAuthId
    ((res,palpiteWidget),enctype) <- runFormPost entryForm
    case res of
        FormSuccess palpite -> do
           let palpite' = palpite { palpiteUserId = maybeCurrentUserId }
           palpiteId <- runDB $ insert palpite'
           setMessage "Palpite criado"
           redirect PalpiteR
        _ -> do
            setMessage "Erro na criacao do palpite"
            redirect PalpiteR


createSettigs str = FieldSettings
    { fsLabel = str, fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
    , fsAttrs =
        [ ("class", "form-control")
        ]
    }

entryForm :: Form Palpite
entryForm = renderBootstrap3 BootstrapBasicForm $ Palpite
    <$> areq   textField (createSettigs "Time da casa") Nothing
    <*> areq   intField (createSettigs "Gols Time da casa") Nothing
    <*> areq   textField (createSettigs "Visitante") Nothing
    <*> areq   intField (createSettigs "Gols Visitante") Nothing
    <*> aopt   hiddenField "userId" Nothing