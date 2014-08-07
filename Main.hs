{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class (liftIO)
import           System.IO
import           System.Directory
import           System.FilePath
import           System.Exit
import           Data.Maybe
import           Aws
import           Aws.S3
import           Network.HTTP.Conduit
import           Data.Attoparsec.Text
import           Esthree.Types
import           Esthree.Parser
import           Esthree.Print
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Welcome to the esthree REPL!"
    putStrLn "  enter '?' for help and 'q' to quit."

    mCreds <- readCreds "default"
    when (isNothing mCreds) $
        putStrLn "Warning: no credentials found in project or home directory."

    let s3Cfg   = defServiceConfig :: S3Configuration NormalQuery
        mkCfg c = Configuration Timestamp c $ defaultLog Warning
        mCfg    = fmap mkCfg mCreds
    withManager $ \mgr -> repl $ Repl mCfg s3Cfg mgr


readCreds :: T.Text -> IO (Maybe Credentials)
readCreds name = do
    pf <- projectCredFile
    pExists <- doesFileExist pf

    hf <- homeCredFile
    hExists <- doesFileExist hf

    if pExists
      then loadCredentialsFromFile pf name
      else if hExists
             then loadCredentialsFromFile hf name
             else return Nothing


homeCredFile :: IO FilePath
homeCredFile = fmap (</> ".aws-keys") getHomeDirectory

projectCredFile :: IO FilePath
projectCredFile = fmap (</> ".aws-keys") getCurrentDirectory

repl :: Repl -> ResourceT IO ()
repl r = do
    str <- liftIO $ do hPutStr stdout "\n> "
                       hFlush stdout
                       hGetLine stdin
    let eCmd = parseOnly s3Command $ T.pack str
    r' <- case eCmd of
              Right cmd -> executeS3Command r cmd
              Left err  -> do liftIO $ putStrLn $ unwords [ str, "did not parse:", err ]
                              return r
    liftIO $ hFlush stdout
    repl r'

executeS3Command :: Repl -> S3Command -> ResourceT IO Repl
executeS3Command r (S3PutBucket bucket loc) =
    fromRepl r (PutBucket bucket Nothing loc) $ const $ okay >> return r
executeS3Command r (S3ListBucket bucket) =
    fromRepl r (getBucket bucket) $ \b -> liftIO $ printBucket b >> return r
executeS3Command r S3ListBuckets =
    fromRepl r GetService $ \s -> do
        liftIO $ printService s
        return r
executeS3Command r S3Help = forM_ availableCmds (liftIO . putStrLn . show) >> return r
executeS3Command r (S3Unknown str) =
    liftIO $ putStrLn (unwords [ "Unknown command", T.unpack str ]) >> return r
executeS3Command _ S3Quit = liftIO $ exitSuccess

fromRepl :: forall a r. (Transaction r a, ServiceConfiguration r ~ S3Configuration) => Repl -> r -> (a -> ResourceT IO Repl) -> ResourceT IO Repl
fromRepl rpl@(Repl (Just cfg) scfg mgr) r f = do
    result <- fmap responseResult $ aws cfg scfg mgr r
    case result of
       Left exc -> do liftIO $ putStrLn $ show exc
                      return rpl
       Right a  -> f a
fromRepl rpl _ _ = do
    liftIO $ putStrLn "You first must load a valid set of credentials."
    return rpl

okay :: ResourceT IO ()
okay = liftIO $ putStrLn "ok"
