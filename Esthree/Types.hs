{-# LANGUAGE OverloadedStrings #-}
module Esthree.Types where

import Aws
import Aws.S3
import Network.HTTP.Conduit
import Data.Text

data Repl = Repl { replConfig     :: Maybe Configuration
                 , replServiceCfg :: S3Configuration NormalQuery
                 , replManager    :: Manager
                 }

availableCmds :: [S3Command]
availableCmds = [ S3Quit
                , S3Help
                , S3ListBuckets
                , S3ListBucket ""
                , S3PutBucket "" ""
                ]

instance Show S3Command where
    show S3Quit = "quit, q - quit esthree"
    show S3Help = "help, ? - show this help"
    show (S3PutBucket _ _) = "putBucket, pb {bucket name} {location} - create a new bucket named {bucket name} at {location}"
    show S3ListBuckets = "listBuckets, lbs - list s3 buckets"
    show (S3ListBucket _) = "listBucket, lb {bucket name} - list objects in {bucket name}"
    show (S3Unknown _) = "command unknown"

data S3Command = S3ListBuckets
               | S3ListBucket Text
               | S3PutBucket Text LocationConstraint
               | S3Help
               | S3Unknown Text
               | S3Quit
