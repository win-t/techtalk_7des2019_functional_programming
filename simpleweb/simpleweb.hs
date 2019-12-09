{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

app :: Application
app req sendResp = sendResp $
  case pathInfo req of
    ["hello"] -> responseBuilder
      status200
      [("Content-Type", "text/plain")]
      "Hello World\n"

    _ -> responseBuilder
      status404
      [("Content-Type", "text/plain")]
      "404 NOT FOUND\n"

main :: IO ()
main = run 8080 app
