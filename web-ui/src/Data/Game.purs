module Lightson.Data.Game 
  ( Game -- We are not exposing the constructor for safety purposes
  , Size
  , Level
  , levelOne
  , levelTwo
  , levelThree
  , nextLevel
  )

  where

import Data.Array ((..))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Newtype
import Prelude ((+), ($), (<$>), (<*>), (>=), otherwise)

type Size = Int
type Level = Int

newtype Game =
  Game { size :: Size
       , level :: Level
       , map :: Map (Tuple Int Int) Boolean
       }

derive instance newTypeGame :: Newtype Game _

nextLevel :: Game -> Maybe Game
nextLevel (Game { size, level, map })
  | level >= 3 = Nothing
  | otherwise  = Just $ generate (level + 2) level

levelOne :: Game 
levelOne = generate 3 1

levelTwo :: Game 
levelTwo = generate 4 2

levelThree :: Game 
levelThree = generate 5 3
  
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
