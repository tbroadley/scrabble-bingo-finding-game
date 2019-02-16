{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad.Loops
import Data.Char
import Data.List
import Data.Text (pack, strip, unpack)
import System.Random
import System.Random.Shuffle
import Text.Read
import Yesod

data App = App { bingos        :: [String],
                 frequentWords :: [String]
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

  common <- lookupGetParam "common"
  let commonBingos = case common of
                       Just c  -> case readMaybe $ unpack c of
                                    Just i  -> intersect bingos $ take i frequentWords
                                    Nothing -> bingos
                       Nothing -> bingos

  if null commonBingos then invalidArgs [pack "common"] else do
    randomIndex <- liftIO $ getStdRandom (randomR (1, length commonBingos - 1))
    let word = commonBingos !! randomIndex
    shuffledWord <- liftIO $ shuffleUntilNotInDictionary commonBingos word

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

clean :: String -> String
clean = unpack . strip . pack

main :: IO ()
main = do
         dictionary <- readFile "dictionary.txt"
         let cleanedDictionary = map clean $ lines dictionary
         let bingos = filter ((== 7) . length) cleanedDictionary

         wordFrequencyList <- readFile "word-frequencies.txt"
         let frequentWords = map (map toUpper . head . words) $ lines wordFrequencyList

         warp 3000 App
           { bingos = bingos,
             frequentWords = frequentWords
           }
