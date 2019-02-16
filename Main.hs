{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Data.Text (pack, strip, unpack)
import System.Random
import Yesod

data App = App { bingos :: [String]
               }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
  App {..} <- getYesod
  randomIndex <- liftIO $ getStdRandom (randomR (1, length bingos))
  defaultLayout [whamlet|
    A random seven-letter Scrabble word: #{bingos !! randomIndex}
  |]

main :: IO ()
main = do
         dictionary <- readFile "dictionary.txt"
         let cleanedDictionary = map (unpack . strip . pack) . lines $ dictionary
         warp 3000 App
           { bingos = filter ((== 7) . length) cleanedDictionary
           }
