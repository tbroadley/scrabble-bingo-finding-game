{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad.Loops
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Text (pack, strip, unpack)
import System.Random
import System.Random.Shuffle
import Text.Read
import Yesod

type WordWithFrequency = (String, Maybe Int)

data App = App { bingos :: [WordWithFrequency]
               }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

shuffleUntilNotInDictionary :: [WordWithFrequency] -> String -> IO String
shuffleUntilNotInDictionary dictionary = iterateUntil (not . (`elem` (map fst dictionary))) . shuffleM

getHomeR :: Handler Html
getHomeR = do
  App {..} <- getYesod

  common <- lookupGetParam "common"
  let commonBingos = case common of
                       Just c  -> case readMaybe $ unpack c of
                                    Just i  -> filter isFrequent bingos
                                                 where
                                                   isFrequent (_, Just f)  = f <= i
                                                   isFrequent (_, Nothing) = False
                                    Nothing -> bingos
                       Nothing -> bingos

  if null commonBingos then invalidArgs [pack "common"] else do
    randomIndex <- liftIO $ getStdRandom (randomR (1, length commonBingos - 1))
    let word = fst $ commonBingos !! randomIndex
    shuffledWord <- liftIO $ shuffleUntilNotInDictionary commonBingos word

    let sortedWord = sort word
    let matchingBingos = filter ((== sortedWord) . sort . fst) bingos

    defaultLayout [whamlet|
      <p>
        These seven letters: #{shuffledWord}
      <p>
        Can be unscrambled into the following bingos:
        <ul>
          $forall bingo <- matchingBingos
            <li>#{fst bingo}
    |]

main :: IO ()
main = do
         dictionary <- readFile "dictionary.txt"
         let cleanedDictionary = map (unpack . strip . pack) $ lines dictionary
         let bingos = filter ((== 7) . length) cleanedDictionary

         wordFrequencyList <- readFile "word-frequencies.txt"
         let frequentWords = Map.fromList . (`zip` [1..]) . map (map toUpper . head . words) $ lines wordFrequencyList

         warp 3000 App
           { bingos = map (\b -> (b, Map.lookup b frequentWords)) bingos
           }
