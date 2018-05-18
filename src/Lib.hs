{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getEnrollmentsUrl 
  , getEnrollments
  --, runTests
  ) where

import Data.Aeson
import Data.Aeson.Types --(Value, parseJSON, parseJSONList, Parser, FromJSON, withObject, (.:))
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Simple
import Network.HTTP.Conduit
import GHC.Generics

data EnrollmentUser = EnrollmentUser { enrollmentUserId :: Integer
                                     , name :: String
                                     , loginId :: String
} deriving (Show)

instance FromJSON EnrollmentUser where
  parseJSON = withObject "EnrollmentUser" $ \u -> EnrollmentUser
    <$> u .: "id"
    <*> u .: "name"
    <*> u .: "login_id"

instance ToJSON EnrollmentUser where
  toJSON e = object [
    "id" .= enrollmentUserId e,
    "name" .= name e,
    "login_id" .= loginId e
    ]

data Enrollment = Enrollment {
    enrollmentId :: Integer
  , user :: EnrollmentUser
  } deriving (Show)

instance FromJSON Enrollment where
  parseJSON = withObject "Enrollment" $ \e -> Enrollment
    <$> e .: "id"
    <*> e .: "user"

instance ToJSON Enrollment where
  toJSON e = object [
    "id" .= enrollmentId e,
    "user" .= user e
    ]

getEnrollmentsUrl :: String -> String -> String
getEnrollmentsUrl domain courseId =
  "https://" ++ domain ++ "/api/v1/courses/" ++ courseId ++ "/enrollments?per_page=500&type=StudentEnrollment"

getEnrollments :: String -> String -> String -> IO ()
getEnrollments domain courseId accessToken = do
  initReq <- parseRequest $ getEnrollmentsUrl domain courseId
  let req = initReq
              { requestHeaders = [("Authorization", S8.pack $ "Bearer " ++ accessToken)]
              }
  response <- httpJSON req :: (IO (Response Value))
  putStrLn $ "The status code: " ++ show (getResponseStatusCode response)
  let body = getResponseBody response
  let result = fromJSON body :: Result [Enrollment]
  case result of
    Error e -> putStrLn $ "error: " ++ e
    Success enrollments ->
      mapM_ printEnrollment enrollments
        where printEnrollment enr = deleteEnrollment domain courseId enr accessToken
  putStrLn "done!"

deleteEnrollmentUrl :: String -> String -> Enrollment -> String
deleteEnrollmentUrl domain courseId enr =
  "https://" ++ domain ++ "/api/v1/courses/" ++ courseId ++ "/enrollments/" ++ show (enrollmentId enr) ++ "?task=delete"

deleteEnrollment :: String -> String -> Enrollment -> String -> IO ()
deleteEnrollment domain courseId enr accessToken = do
  putStrLn $ "attempting to delete enrollment for " ++ loginId (user enr)
  initReq <- parseRequest $ deleteEnrollmentUrl domain courseId enr
  let req = initReq {
    requestHeaders = [("Authorization", S8.pack $ "Bearer " ++ accessToken)],
    method = "DELETE"
    }
  response <- httpJSON req :: (IO (Response Value))
  let statusCode = getResponseStatusCode response
  if statusCode >= 400 then
    putStrLn $ "  (Error - " ++ (show statusCode) ++ ")"
  else
    putStrLn "...deleted"
