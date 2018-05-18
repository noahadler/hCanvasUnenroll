module Main where

import Lib

import System.IO

import Data.Maybe (fromMaybe)

prompt :: String -> Maybe String -> IO String
prompt text defaultValue = do
  putStr $ case defaultValue of
    Just s -> text ++ " (" ++ s ++ "): "
    Nothing -> text ++ ": "
  hFlush stdout
  x <- getLine
  return $ case x of
    "" -> fromMaybe "" defaultValue
    _ -> x

main :: IO ()
main = do
  domain <- prompt "Domain" (Just "uk.instructure.com")
  courseId <- prompt "Course ID" Nothing
  accessToken <- prompt "Access token" Nothing
  getEnrollments domain courseId accessToken
