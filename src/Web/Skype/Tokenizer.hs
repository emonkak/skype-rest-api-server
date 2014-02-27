module Web.Skype.Tokenizer where

import Data.Char (isSpace)

import qualified Data.Text as T

ngram :: Int -> T.Text -> [T.Text]
ngram n = filter ((==) n . T.foldl' step 0) . map (T.take n) . T.tails
  where
    step acc c
      | isSpace c = acc
      | otherwise = acc + 1

bigram :: T.Text -> [T.Text]
bigram = ngram 1

unigram :: T.Text -> [T.Text]
unigram = ngram 2

trigram :: T.Text -> [T.Text]
trigram = ngram 3
