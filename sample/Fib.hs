--module Fib  where

import CGI
import Text.Printf

main = runCGI appMain

appMain :: HTTPRequest -> IO HTTPResponse
appMain req =
  case lookupVar "ok" req of
    Nothing -> return $ HTTPResponse "text/plain" "Hello, world!"
    Just n -> return $ HTTPResponse "text/plain" (appFib n)

appFib :: String -> String
appFib x = printf "{fib: %d}" $ fib (read x)
  where
    fib :: Int -> Integer
    fib n = fibs !! n
    fibs :: [Integer]
    fibs = 0:1:zipWith(+) fibs (tail fibs)

-- showw :: HTTPRequest -> String
-- showw (HTTPRequest h)
