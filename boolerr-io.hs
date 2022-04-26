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

data Summary = Summary
  { title :: Maybe String,
    ok :: Bool
  }
  deriving (Show)

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
  docOrError <- readDoc url
  return $ case docOrError of
    Left err -> Summary {title = Nothing, ok = False}
    Right doc -> buildSummary doc

readAndBuildSummary' :: String -> IO Summary
readAndBuildSummary' url =
  readDoc url >>= \docOrError -> return $ case docOrError of
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

isTitleNonEmpty'' :: Doc -> Maybe Bool
isTitleNonEmpty'' doc = not . null <$> (doc.head >>= (.title))

-- readWhetherTitleNonEmpty

readWhetherTitleNonEmpty :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty url = do
  docOrError <- readDoc url
  return $ do
    doc <- docOrError
    return $ isTitleNonEmpty doc

readWhetherTitleNonEmpty' :: String -> IO (Either Error (Maybe Bool))
readWhetherTitleNonEmpty' url =
  readDoc url >>= \docOrError ->
    return $ docOrError >>= \doc -> return $ isTitleNonEmpty doc

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
    -- let hasTitleSure = either (const False) id $ fromMaybe False <$> hasTitle
    let hasTitleSure = fromMaybe False $ either (const Nothing) id hasTitle
    putStrLn $
      printf "  Has title: %s vs %s" (show hasTitle) (show hasTitleSure)
