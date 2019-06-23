module Lib where

import Control.Monad (mfilter, (>=>))
import qualified Data.Map as M (lookup, fromList)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Either.Extra (maybeToEither)
import Data.String.Utils (strip)
import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.Aeson (FromJSON, ToJSON)
import Aws.Lambda.Runtime (Context)

data Request = Request { input :: String } deriving (Generic)
instance FromJSON Request

data Response = Response { result :: Double } deriving (Generic)
instance ToJSON Response

handler :: Request -> Context -> IO (Either String Response)
handler request context =
  return (fmap Response $ processInput $ input request) 

processInput :: String -> Either String Double
processInput= lookupText >=> tryConvert
  where
    lookupText :: String -> Either String String
    lookupText  = maybeToEither "no text"
                . (mfilter (not . null))
                . (fmap (strip . unpack))
                . M.lookup "text"
                . M.fromList
                . parseSimpleQuery
                . pack

    tryConvert :: String -> Either String Double
    tryConvert  = maybeToEither "couldn't convert"
                . fmap fahrenheitToCelsius
                . readMaybe

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius d = (d - 32) * 5 / 9
