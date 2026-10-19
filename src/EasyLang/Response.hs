{-# LANGUAGE OverloadedStrings #-}

module EasyLang.Response
  ( parseTranslation
  , apiErrorMessage
  ) where

import Data.Aeson (Value (..), eitherDecode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text

-- A successful HTTP status alone does not establish that a translation finished.
-- Only accept a normal stop, and never return partial output as a success.
parseTranslation :: LBS.ByteString -> Either Text Text
parseTranslation body =
  case eitherDecode body of
    Left _ -> Left "OpenRouter returned invalid JSON. Retry the request."
    Right value ->
      case parseEither parseResponse value of
        Left _ -> Left "OpenRouter returned an unexpected response format. Try another model."
        Right result -> result

-- Extract only the provider's message; callers should never print the raw body.
apiErrorMessage :: LBS.ByteString -> Maybe Text
apiErrorMessage body =
  case eitherDecode body of
    Left _ -> Nothing
    Right value ->
      case parseMaybe (withObject "response" (.:? "error")) value of
        Just (Just apiError) -> errorMessage apiError
        _ -> Nothing

parseResponse :: Value -> Parser (Either Text Text)
parseResponse = withObject "response" $ \response -> do
  apiError <- response .:? "error"
  case apiError of
    Just value -> pure . Left $
      maybe "OpenRouter returned an API error. Retry the request."
        ("OpenRouter API error: " <>)
        (errorMessage value)
    Nothing -> do
      choices <- response .: "choices" :: Parser [Value]
      case choices of
        [] -> pure (Left "OpenRouter returned no translation choices. Try another model.")
        firstChoice : _ -> parseChoice firstChoice

parseChoice :: Value -> Parser (Either Text Text)
parseChoice = withObject "choice" $ \choice -> do
  finishReason <- choice .:? "finish_reason" :: Parser (Maybe Text)
  case finishReason of
    Just "stop" -> choice .: "message" >>= parseMessage
    Just "length" -> pure (Left "The translation was truncated. Increase --max-tokens or translate a shorter passage.")
    Just "content_filter" -> pure (Left "The provider blocked the translation with a content filter. Try another model.")
    Just "error" -> pure (Left "The provider failed before completing the translation. Retry the request.")
    Just "tool_calls" -> pure (Left toolCallError)
    Just "function_call" -> pure (Left toolCallError)
    Nothing -> pure (Left "OpenRouter did not confirm a complete translation (missing finish_reason). Retry the request.")
    Just _ -> pure (Left "OpenRouter returned an unsupported completion status. Try another model.")
  where
    toolCallError = "The model requested a tool instead of completing the translation. Try another model."

parseMessage :: Value -> Parser (Either Text Text)
parseMessage = withObject "message" $ \message -> do
  refusal <- message .:? "refusal" :: Parser (Maybe Text)
  case refusal of
    Just reason | not (Text.null (Text.strip reason)) ->
      pure (Left "The model refused the translation. Try another model.")
    _ -> do
      content <- message .:? "content" :: Parser (Maybe Text)
      pure $ case content of
        Just translation | not (Text.null (Text.strip translation)) -> Right translation
        _ -> Left "OpenRouter returned no translation text. Try another model."

errorMessage :: Value -> Maybe Text
errorMessage (String message) = nonempty message
errorMessage value = do
  message <- parseMaybe (withObject "error" (.: "message")) value
  nonempty message

nonempty :: Text -> Maybe Text
nonempty message
  | Text.null (Text.strip message) = Nothing
  | otherwise = Just message
