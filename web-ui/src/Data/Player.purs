module Lightson.Data.Player where

type PlayerId = Int
type Username = String
type Level = String

type Player = 
  { username :: Username
  , score :: Int
  , level :: String
  }

_username :: Player -> String
_username { username } = username
