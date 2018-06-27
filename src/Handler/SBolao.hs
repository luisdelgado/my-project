{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.SBolao where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getSBolaoR :: BolaoId -> Handler Html
getSBolaoR bolaoId = do
    (userId, user) <- requireAuthPair
    palpites <- runDB $ selectList [PalpiteBolao ==. bolaoId] [Asc PalpiteId]
    (palpiteWidget, enctype) <- generateFormPost (entryForm bolaoId userId (userIdent user))
    defaultLayout $ do
        setTitle "Palpites"
        $(widgetFile "palpites")

postSBolaoR :: BolaoId -> Handler Html
postSBolaoR bolaoId = do
    (userId, user) <- requireAuthPair
    ((res,palpiteWidget),enctype) <- runFormPost (entryForm bolaoId userId (userIdent user))
    case res of
        FormSuccess palpite -> do
           --let palpite' = palpite { palpiteUser = userId, palpiteUserName = (userIdent user) }
           palpiteId <- runDB $ insert palpite
           setMessage "Palpite criado"
           redirect $ SBolaoR bolaoId
        _ -> do
            setMessage "Erro na criacao do palpite"
            redirect $ SBolaoR bolaoId


entryForm :: BolaoId -> UserId -> Text -> Form Palpite
entryForm bolaoId userId userName = renderBootstrap3 BootstrapBasicForm $ Palpite
    <$> areq   intField (createSettigs "Gols Time da casa") Nothing
    <*> areq   intField (createSettigs "Gols Visitante") Nothing
    <*> pure bolaoId
    <*> pure userId
    <*> pure userName


createSettigs str = FieldSettings
    { fsLabel = str, fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
    , fsAttrs =
        [ ("class", "form-control")
        ]
    }