{-# LANGUAGE OverloadedStrings #-}

module EasyLang.LanguagePairs
  ( LanguagePair (..)
  , validatePairs
  , parsePairSpecs
  , parsePairList
  , partnerFor
  ) where

import Control.Monad (foldM)
import Data.Char (isAscii)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data LanguagePair = LanguagePair Text Text
  deriving (Eq, Show)

-- Regional variants share one routing language: en and en-US cannot occur in
-- different pairs, or on opposite sides of the same pair.
validatePairs :: [(Text, Text)] -> Either Text [LanguagePair]
validatePairs entries = do
  pairs <- traverse normalizePair entries
  _ <- foldM checkLanguage Set.empty (concatMap languages pairs)
  pure pairs
  where
    normalizePair (first, second) = LanguagePair <$> normalizeLanguage first <*> normalizeLanguage second
    languages (LanguagePair first second) = [first, second]
    checkLanguage seen language
      | primaryLanguage language `Set.member` seen =
          Left ("Conflicting language pairs: language \"" <> primaryLanguage language
            <> "\" appears more than once (including regional variants). Each language may belong to only one pair.")
      | otherwise = Right (Set.insert (primaryLanguage language) seen)

-- Each specification describes both directions; no default pairs are added.
parsePairSpecs :: [Text] -> Either Text [LanguagePair]
parsePairSpecs specs = traverse splitPair specs >>= validatePairs
  where
    splitPair spec = case T.splitOn ":" spec of
      [first, second] -> Right (first, second)
      _ -> Left ("Invalid language pair \"" <> spec <> "\". Use two language codes separated by ':', for example zh:en.")

parsePairList :: Text -> Either Text [LanguagePair]
parsePairList value
  | T.null (T.strip value) = Right []
  | otherwise = parsePairSpecs (T.splitOn "," value)

-- Match source variants by their primary language and preserve the configured
-- destination variant. The caller supplies pairs returned by validation.
partnerFor :: [LanguagePair] -> Text -> Maybe Text
partnerFor pairs source = case normalizeLanguage source of
  Left _ -> Nothing
  Right normalized -> lookupPartner (primaryLanguage normalized) pairs
  where
    lookupPartner _ [] = Nothing
    lookupPartner language (LanguagePair first second : rest)
      | language == primaryLanguage first = Just second
      | language == primaryLanguage second = Just first
      | otherwise = lookupPartner language rest

normalizeLanguage :: Text -> Either Text Text
normalizeLanguage original =
  let stripped = T.strip original
      normalized = T.toLower (T.replace "_" "-" stripped)
  in case T.splitOn "-" normalized of
    primary : subtags
      | T.all isAscii stripped
      , T.length primary == 2
      , T.all asciiLetter primary
      , all validSubtag subtags -> Right normalized
    _ -> Left ("Invalid language code \"" <> original
      <> "\". Use two-letter ASCII language codes with optional regional/script subtags, for example zh:en or zh-cn:en-us.")
  where
    asciiLetter character = character >= 'a' && character <= 'z'
    asciiDigit character = character >= '0' && character <= '9'
    validSubtag subtag = T.length subtag >= 2 && T.length subtag <= 8
      && T.all (\character -> asciiLetter character || asciiDigit character) subtag

primaryLanguage :: Text -> Text
primaryLanguage = T.takeWhile (/= '-')
