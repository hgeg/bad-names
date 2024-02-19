{-# LANGUAGE TypeOperators, OverloadedStrings, DataKinds #-}
module Names where

import           Control.Monad.IO.Class as IO

import           Servant
import           Servant.HTML.Blaze

import           System.Random

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html5 (Html, toHtml, docTypeHtml, (!))
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

type API = "names"                     :> Get '[HTML] Html
      :<|> "names" :> Capture "id" Int :> Get '[HTML] Html

type Name = (Int, String)

server :: Server API
server = randomHandler :<|> idHandler

randomHandler :: Handler Html
randomHandler = do
    name <- IO.liftIO getRandomName
    return . page $ name

idHandler :: Int -> Handler Html
idHandler id = do
    name <- IO.liftIO . getNameWithId $ id
    return . page $ name

getRandomName :: IO Name
getRandomName = do
    content <- readFile "names.txt";
    let names = lines content
    let count = length names
    randomId <- randomRIO (0, count - 1)
    return (randomId+1, names !! randomId)

clamp :: (Int, Int) -> Int -> Int
clamp (s,e) = min e . max s

getNameWithId :: Int -> IO Name
getNameWithId id = do
    content <- readFile "names.txt";
    let names = lines content
    let safeId = clamp (0, length names) (id-1)
    case drop safeId names of
        [] -> getRandomName
        (n:_) -> return (id, n)

page :: Name -> Html
page (order, name) = docTypeHtml $ do
  H.head $ do
    H.title "Kötü Isimler"
    H.style $ do
      "body{background:#fafafa;margin:0;padding:0;display:flex;justify-content:center;align-items:center;height:100vh}"
      ".text{opacity:0;animation:fade-in .5s ease-in forwards;color:#444;text-align:center;font: bold 48px "Helvetica Neue",Helvetica,Arial,sans-serif; padding: 120px}"
      ".order{color: #aaa;}"
      "@keyframes fade-in{0%{opacity:0}100%{opacity:1}}"
  H.body $ do
    H.div ! A.class_ "text" $ do
      H.span ! A.class_ "order" $ toHtml ("#" ++ show (order+1) ++ " ")
      H.span $ toHtml name
