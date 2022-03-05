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

type Error = String

data Doc = Doc {head :: Maybe Head} deriving (Show)

data Head = Head {title :: Maybe Error} deriving (Show)

data Summary = Summary {title :: Maybe Error, ok :: Bool} deriving (Show)

readDoc :: Error -> IO (Either Error Doc)
readDoc url =
  return
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
                      Doc
                        { head =
                            Just Head {title = Just $ printf "Title of %s" url}
                        }

buildSummary :: Doc -> Summary
buildSummary doc =
  Summary {title = doc.head >>= (.title), ok = True}

readAndBuildSummary :: Error -> IO Summary
readAndBuildSummary url = do
  doc <- readDoc url
  return $ case doc of
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

readAndBuildSummary' :: Error -> IO Summary
readAndBuildSummary' url =
  readDoc url >>= \doc -> return $ case doc of
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

readAndBuildSummary'' :: Error -> IO Summary
readAndBuildSummary'' url =
  readDoc url <&> \case
    Left err -> Summary {title = Nothing, ok = True}
    Right doc -> buildSummary doc

isTitleNonEmpty :: Doc -> Maybe Bool
isTitleNonEmpty doc = do
  head <- doc.head
  title <- head.title
  return $ not $ null title

isTitleNonEmpty' :: Doc -> Maybe Bool
isTitleNonEmpty' doc = not <$> null <$> (doc.head >>= (.title))

readWhetherTitleNonEmpty :: Error -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty url = do
  maybeDoc <- readDoc url
  return $ do
    doc <- maybeDoc
    return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty' :: Error -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty' url =
  readDoc url >>= \maybeDoc ->
    return $ maybeDoc >>= \doc -> return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty'' :: Error -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty'' url = (isTitleNonEmpty <$>) <$> readDoc url

readWhetherTitleNonEmpty''' :: Error -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty''' url = readDoc url <&> (<&> isTitleNonEmpty)

main :: IO ()
main = do
  let urls = ["good", "title-empty", "title-missing", "head-missing", "fail"]
  forM_ urls $ \url -> do
    putStrLn $ printf "Checking \"https://%s/\":" url
    summary <- readAndBuildSummary url
    putStrLn $ printf "  Summary: %s" $ show summary
    putStrLn $ printf "  Title: %s" $ fromMaybe "" summary.title
    hasTitle <- readWhetherTitleNonEmpty url
    let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle
    putStrLn $
      printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)
