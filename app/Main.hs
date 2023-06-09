module Main where

import Names
import Data.Proxy
import Network.Wai.Handler.Warp

import Servant
import System.Environment

api :: Proxy API
api = Proxy

main :: IO ()
main = do
    port <- (read . head <$> getArgs) :: IO Int
    putStrLn "running server"
    run port $ serve api server

