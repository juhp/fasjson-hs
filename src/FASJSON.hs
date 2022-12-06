{-# LANGUAGE CPP, OverloadedStrings #-}

-- SPDX-License-Identifier: BSD-3-Clause

module FASJSON (
  fedoraFASJSON,
  queryFasjson,
  noData,
  Object
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Network.Curl.Aeson
import Network.Curl.Opts

fedoraFASJSON :: String
fedoraFASJSON = "https://fasjson.fedoraproject.org/v1"

-- | Query fasjson web API
--
-- See <https://fasjson.fedoraproject.org/docs/v1/>
queryFasjson :: (ToJSON a, FromJSON b) => String -> String -> Maybe a -> IO b
queryFasjson service path mparams = do
  obj <- curlAesonCustom [CurlHttpAuth [HttpAuthGSSNegotiate],
                          CurlUserPwd ":",
                          CurlFailOnError False]
         "GET" (service +/+ path) mparams
  case lookupKey "result" obj of
    Just res -> return res
    Nothing ->
      case lookupKey "message" obj of
        Just msg -> errorWithoutStackTrace msg
        Nothing -> error $ "unknown response: " ++ show obj

-- taken from http-query

-- | Combine two path segments with a slash
--
-- > "abc" +/+ "def" == "abc/def"
-- > "abc/" +/+ "def" == "abc/def"
-- > "abc" +/+ "/def" == "abc/def"
infixr 5 +/+
(+/+) :: String -> String -> String
"" +/+ s = s
s +/+ "" = s
s +/+ t | last s == '/' = s ++ t
        | head t == '/' = s ++ t
s +/+ t = s ++ '/' : t

-- | Look up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k =
  parseMaybe (.: fromText k)

#if !MIN_VERSION_aeson(2,0,0)
fromText :: Text -> Text
fromText = id
#endif
