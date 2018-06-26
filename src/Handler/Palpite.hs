module Handler.Palpite where

import Import

getPalpiteR :: Handler Value
getPalpiteR = do
    palpites <- runDB $ selectList [] [Asc PalpiteId]
    returnJson palpites

postPalpiteR :: Handler Value
postPalpiteR = do
    palpite <- (requireJsonBody :: Handler Palpite)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let palpite' = palpite { palpiteUserId = maybeCurrentUserId }

    insertedPalpite <- runDB $ insertEntity palpite'
    returnJson insertedPalpite
