{-# LANGUAGE OverloadedStrings #-}

module Challenge ( runChallenge) where

import Text.Blaze.XHtml5 hiding (map)
import Text.Blaze.XHtml5.Attributes (src)
import System.Directory
import Text.Blaze.Html.Renderer.String
import Control.Monad
import System.Random
import Data.Time.Clock
import Chart

class DualPlace a where
  url :: a -> String
  url a = baseUrl ++ "/" ++ piece a
  
  path :: a -> FilePath
  path a = baseDir ++ "/" ++ piece a
  
  piece :: a -> String

baseDir = "/home/lebedev/nginx/static/challenge"
baseUrl = "http://static.smartblobs.org/challenge"

instance DualPlace Challenge where
  piece (Challenge cid) = cid ++ ".html"

instance DualPlace Image where
  piece (Image c cid) = challengeCID c ++ "/" ++ cid ++ ".png"

newChallenge :: IO Challenge
newChallenge = do
  t <- getCurrentTime 
  let t' = map (\s -> case s of
                   ' ' -> '_'
                   v -> v) $ show t
  return $ Challenge t'
  
data Challenge = Challenge { challengeCID :: CID } deriving (Show)

type CID = String

newImage :: Challenge -> IO Image
newImage challenge = do
  cid <- replicateM 10 $ randomRIO ('a','z')
  return $ Image challenge cid

data Image = Image Challenge CID

runChallenge :: (Plotter -> IO Html) -> IO ()
runChallenge action = do
  challenge <- newChallenge
  createDirectory $ baseDir ++ "/" ++ challengeCID challenge
  let plotter ps = do
        img <- newImage challenge
        gplotFile (path img) ps
        return $ src (toValue $ url img)
  html <- action plotter
  writeFile (path challenge) $ renderHtml html
  print $ url challenge


type Plotter = [(Double, Double)] -> IO Attribute

