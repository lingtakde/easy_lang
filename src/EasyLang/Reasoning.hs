{-# LANGUAGE OverloadedStrings #-}

module EasyLang.Reasoning (parseReasoningEffort) where

import Data.Text (Text)
import qualified Data.Text as T

parseReasoningEffort :: Text -> Either Text Text
parseReasoningEffort value =
  let normalized = T.toLower (T.strip value)
  in if normalized `elem` ["none", "minimal", "low", "medium", "high", "xhigh", "max"]
      then Right normalized
      else Left "reasoning_effort must be one of: none, minimal, low, medium, high, xhigh, max."
