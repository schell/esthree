{-# LANGUAGE OverloadedStrings #-}
module Esthree.Parser where

import Data.Attoparsec.Text
import Control.Applicative
import Esthree.Types
import Aws.S3

s3Command :: Parser S3Command
s3Command = listBucketsCmd <|>
            listBucketCmd  <|>
            putBucketCmd   <|>
            helpCmd        <|>
            quitCmd        <|>
            unknownCmd

putBucketCmd :: Parser S3Command
putBucketCmd = do
    _ <- string "putBucket" <|> string "pb"
    skipSpace
    name <- takeWhile1 (/= ' ')
    skipSpace
    loc <- location
    return $ S3PutBucket name loc

location :: Parser LocationConstraint
location = west       <|>
           west2      <|>
           eu         <|>
           southeast  <|>
           southeast2 <|>
           northeast  <|>
           classic
    where classic = return ""
          west = "us-west-1"
          west2 = "us-west-2"
          eu = "EU"
          southeast = "ap-southeast-1"
          southeast2 = "ap-southeast-2"
          northeast = "ap-northeast-1"



listBucketsCmd :: Parser S3Command
listBucketsCmd = S3ListBuckets <$ (string "listBuckets" <|> string "lbs")

listBucketCmd :: Parser S3Command
listBucketCmd = do
    _ <- string "listBucket" <|> string "lb"
    b <- takeText
    return $ S3ListBucket b

helpCmd :: Parser S3Command
helpCmd = S3Help <$ (string "help" <|> string "?")

unknownCmd :: Parser S3Command
unknownCmd = S3Unknown <$> takeText

quitCmd :: Parser S3Command
quitCmd = S3Quit <$ (string "quit" <|> string "q")
