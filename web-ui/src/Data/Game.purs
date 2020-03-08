module Lightson.Data.Game 
  ( Game -- We are not exposing the constructor for safety purposes
  , Size
  , Level
  , levelOne
  , nextLevel
  , _size
  , isOn
  , switchN
  , isWin
  )

  where

import Data.Array ((..))
import Data.List (all)
import Data.Map (Map, fromFoldable, update, lookup, values)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Prelude

type Size = Int
type Level = Int

newtype Game =
  Game { size :: Size
       , level :: Level
       , map :: Map (Tuple Int Int) Boolean
       }

_size :: Game -> Size
_size (Game { size, level: _, map: _ }) = size

_map :: Game -> Map (Tuple Int Int) Boolean
_map (Game game) = game.map

nextLevel :: Game -> Maybe Game
nextLevel (Game { size, level, map })
  | level >= 3 = Nothing
  | otherwise  = Just $ generate (size + 1) (level + 1)

levelOne :: Game 
levelOne = generate 3 1

isOn :: Tuple Int Int -> Game -> Boolean
isOn key =
  maybe false identity
    <<< lookup key
    <<< _map

-- | Check if all of the lights (cells) are turned on.
isWin :: Game -> Boolean
isWin = all ((==) true) <<< values <<< _map

-- | Turn on / off the lights at the given coordinates
-- | and all of its neighbours
switchN :: Tuple Int Int -> Game -> Maybe Game
switchN (Tuple x y) game = 
  switch (Tuple x y) game 
  >>= switch (Tuple (x + 1) y)
  >>= switch (Tuple (x - 1) y)
  >>= switch (Tuple x (y - 1))
  >>= switch (Tuple x (y + 1))


-- | Turn on / off the lights at the given coordinates.
-- | Leftmost coordinate starts with 1 (not 0)
switch :: Tuple Int Int -> Game -> Maybe Game
switch key (Game game) =
  let 
    turnOn'or'Off = (\bool -> Just $ not bool)
  in 
    Just $ Game 
      { size: game.size
      , level: game.level
      , map: update turnOn'or'Off key game.map
      }

generate 
  :: Size 
  -> Level
  -> Game
generate size level =
  let keys = Tuple <$> (1 .. size) <*> (1 .. size)
  in  Game 
        { size 
        , level
        , map: fromFoldable $ (\k -> Tuple k false) <$> keys
        }
