module Server
  ( server
  ) where

import qualified Persistence.Session as S
import Web.Routes (routes)
import Web.Scotty.Trans (scottyT)

server :: IO ()
server = scottyT 3000 runIO routes
  where
    runIO :: S.Session a -> IO a
    runIO session = do
      Right conn <- S.connection
      Right result <- S.run session conn
      return result
