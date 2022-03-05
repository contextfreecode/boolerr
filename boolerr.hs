{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Text.Printf (printf)

data Doc = Doc {head :: Maybe Head} deriving (Show)

data Head = Head {title :: Maybe String} deriving (Show)

data Summary = Summary {title :: Maybe String, ok :: Bool} deriving (Show)

readDoc :: String -> Either String Doc
readDoc url =
  if
      | isInfixOf "fail" url -> Left "Failed to read document"
      | otherwise ->
          Right $
            if
                | isInfixOf "head-missing" url -> Doc {head = Nothing}
                | isInfixOf "title-missing" url ->
                    Doc {head = Just Head {title = Nothing}}
                | isInfixOf "title-empty" url ->
                    Doc {head = Just Head {title = Just ""}}
                | otherwise ->
                    Doc {head = Just Head {title = Just $ printf "Title of %s" url}}

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
  forM_ urls $ \url -> do
    let doc = readDoc url
    putStrLn $ printf "Checking \"%s\":" url
    putStrLn $ show doc
    -- putStrLn $ printf "Hi: %s %s" (show doc) (show $ buildSummary Doc {head = Nothing})
