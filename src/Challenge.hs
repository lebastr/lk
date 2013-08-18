{-# LANGUAGE OverloadedStrings #-}

module Challenge ( runChallenge, def, Config(..)) where

import Text.Blaze.XHtml5 hiding (map)
import Text.Blaze.XHtml5.Attributes (src)
import System.Directory
import Text.Blaze.Html.Renderer.String
import Control.Monad
import System.Random
import Data.Time.Clock
import Chart

data Config = Config { baseDir :: String
                     , baseUrl :: String }

def :: Config
def = Config { baseDir = "/home/lebedev/nginx/static/challenge"
             , baseUrl = "http://static.smartblobs.org/challenge" }


type Plotter = (Double, Double) -> [(Double, Double)] -> IO Attribute


class DualPlace a where
  url :: Config -> a -> String
  url cfg a = baseUrl cfg ++ "/" ++ piece a

  path :: Config -> a -> FilePath
  path cfg a = baseDir cfg ++ "/" ++ piece a

  piece :: a -> String


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

runChallenge :: Config -> String -> (Plotter -> IO Html) -> IO ()
runChallenge cfg challengeTitle action = do
  challenge <- newChallenge
  createDirectory $ baseDir cfg ++ "/" ++ challengeCID challenge
  let plotter (x,y) ps = do
        img <- newImage challenge
        gplotFile (path cfg img) (x,y) ps
        return $ src (toValue $ url cfg img)
  html <- action plotter
  let html' = docTypeHtml $ do
        title $ toHtml challengeTitle
        body html

  writeFile (path cfg challenge) $ renderHtml html'
  print $ url cfg challenge


