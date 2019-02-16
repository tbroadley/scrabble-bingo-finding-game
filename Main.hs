{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad.Loops
import Data.List
import Data.Text (pack, strip, unpack)
import System.Random
import System.Random.Shuffle
import Yesod

data App = App { bingos :: [String]
               }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

shuffleUntilNotInDictionary :: [String] -> String -> IO String
shuffleUntilNotInDictionary dictionary = iterateUntil (not . (`elem` dictionary)) . shuffleM

getHomeR :: Handler Html
getHomeR = do
  App {..} <- getYesod

  randomIndex <- liftIO $ getStdRandom (randomR (1, length bingos))
  let word = bingos !! randomIndex
  shuffledWord <- liftIO $ shuffleUntilNotInDictionary bingos word

  let sortedWord = sort word
  let matchingBingos = filter ((== sortedWord) . sort) bingos

  defaultLayout [whamlet|
    <p>
      These seven letters: #{shuffledWord}
    <p>
      Can be unscrambled into the following bingos:
      <ul>
        $forall bingo <- matchingBingos
          <li>#{bingo}
  |]

main :: IO ()
main = do
         dictionary <- readFile "dictionary.txt"
         let cleanedDictionary = map (unpack . strip . pack) $ lines dictionary
         let bingos = filter ((== 7) . length) cleanedDictionary

         warp 3000 App
           { bingos = bingos
           }
