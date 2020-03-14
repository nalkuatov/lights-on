module Lightson.Api.Request.Verb 
  ( Verb
  , body
  , method
  )
  where

import Data.Argonaut.Core (Json)
import Data.HTTP.Method (Method)
import Data.HTTP.Method as HTTP
import Data.Maybe (Maybe(..))

data Verb
  = Get
  | Post (Maybe Json)

body :: Verb -> Maybe Json
body = case _ of
  Get       -> Nothing
  Post json -> json

method :: Verb -> Method
method = case _ of 
  Get    -> HTTP.GET
  Post _ -> HTTP.POST
