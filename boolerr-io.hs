{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data Error = Error (String) deriving (Show)

data Doc = Doc {head :: Maybe Head}

data Head = Head {title :: Maybe String}

data Summary = Summary {title :: Maybe String, ok :: Bool} deriving (Show)

-- readDoc

readDoc :: String -> IO (Either Error Doc)
readDoc url =
  return
    if
        | isInfixOf "fail" url -> Left $ Error $ printf "Bad read of %s" url
        | otherwise ->
            Right $
              if
                  | isInfixOf "head-missing" url -> Doc {head = Nothing}
                  | isInfixOf "title-missing" url ->
                      Doc {head = Just Head {title = Nothing}}
                  | isInfixOf "title-empty" url ->
                      Doc {head = Just Head {title = Just ""}}
                  | otherwise ->
                      Doc
                        { head =
                            Just Head {title = Just $ printf "Title of %s" url}
                        }

-- buildSummary

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

-- readAndBuildSummary

readAndBuildSummary :: String -> IO Summary
readAndBuildSummary url = do
  doc <- readDoc url
  return $ case doc of
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

readAndBuildSummary' :: String -> IO Summary
readAndBuildSummary' url =
  readDoc url >>= \doc -> return $ case doc of
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

readAndBuildSummary'' :: String -> IO Summary
readAndBuildSummary'' url =
  readDoc url <&> \case
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

-- isTitleNonEmpty

isTitleNonEmpty :: Doc -> Maybe Bool
isTitleNonEmpty doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

isTitleNonEmpty' :: Doc -> Maybe Bool
isTitleNonEmpty' doc = not <$> null <$> (doc.head >>= (.title))

-- readWhetherTitleNonEmpty

readWhetherTitleNonEmpty :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty url = do
  maybeDoc <- readDoc url
  return $ do
    doc <- maybeDoc
    return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty' url =
  readDoc url >>= \maybeDoc ->
    return $ maybeDoc >>= \doc -> return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty'' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty'' url = (isTitleNonEmpty <$>) <$> readDoc url

readWhetherTitleNonEmpty''' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty''' url = readDoc url <&> (<&> isTitleNonEmpty)

-- main

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url
    -- Summary.
    summary <- readAndBuildSummary url
    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title
    -- Has title.
    hasTitle <- readWhetherTitleNonEmpty url
    let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle
    putStrLn $
      printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)
