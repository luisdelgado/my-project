{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.FimBolao where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

postFimBolaoR :: BolaoId -> Handler Html
postFimBolaoR bolaoId = do
    ((res,bolaoWidget),enctype) <- runFormPost entryForm
    case res of
        FormSuccess bolao -> do
            palpites <- runDB $ selectList [PalpiteBolao ==. bolaoId] [Asc PalpiteId]
            defaultLayout $ do
                setTitle "Resultado"
                $(widgetFile "fimbolao")
        _ -> do
            setMessage "Erro na avaliacao do bolão"
            redirect $ SBolaoR bolaoId
        

data Resultado = Resultado
    { golsA      :: Int
    , golsB      :: Int
    }
    deriving Show

entryForm :: Form Resultado
entryForm  = renderBootstrap3 BootstrapBasicForm $ Resultado
    <$> areq   intField (createSettigs "Gols Time da casa") Nothing
    <*> areq   intField (createSettigs "Gols Visitante") Nothing


createSettigs str = FieldSettings
    { fsLabel = str, fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
    , fsAttrs =
        [ ("class", "form-control")
        ]
    }