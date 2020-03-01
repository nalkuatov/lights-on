{-# LANGUAGE OverloadedStrings #-}

module Web.Routes 
  ( routes
  )
  where

import Web.Scotty
import Data.Text.Lazy

routes :: ScottyM ()
routes = do

  get "/" $
    text $ "root"
  
  get "/scores" $
    text $ "scores"

  get "/scores/:id" $ do
    scoreId <- param "id"
    json $ "score's id is " ++ show (scoreId :: Int)
