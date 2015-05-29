{-# LANGUAGE ScopedTypeVariables, OverloadedLists #-}
import Test.Tasty
import Test.Tasty.HUnit

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char
import Data.Monoid
import Data.Loc
import Control.Exception
import Control.DeepSeq

ws = whitespace $ longest $ some (psym isSpace)
-- this is bad, because it accepts an empty string
badWhitespace = whitespace $ longest $ many (psym isSpace)

word = longestToken $ many $ psym isAlpha

longestToken = token . longest

tokens l n s = streamToList $ runLexer l n s
tokensEither l n s = streamToEitherList $ runLexer l n s

unloc (L l a) = (a, l)

-- This recognizes C-style block comments like /* ... */,
-- but also matching delimiters like /*** ... ***/ (to make it more fun)
blockComment = token $
  longestShortest
    (,)
    ((++) <$> string "/" <*> many (sym '*'))
    (\start -> (++) <$> many anySym <*> string (reverse start))

main = defaultMain $ testGroup "Tests"
  [ testCase "Empty string" $
      tokens (longestToken (empty :: RE Char Int) <> whitespace mempty) "-" "" @=? []
  , testCase "Space- and newline-separated numbers" $
      unloc <$> tokens (longestToken decimal <> ws) "-" "1\n 23  456" @?=
      [ (1,  Loc (Pos "-" 1 1 0) (Pos "-" 1 1 0))
      , (23, Loc (Pos "-" 2 2 3) (Pos "-" 2 3 4))
      , (456,Loc (Pos "-" 2 6 7) (Pos "-" 2 8 9))
      ]
  , testCase "Nullable parser, no error" $ do
      let r = tokensEither (longestToken decimal <> badWhitespace) "-" "31 45"
      case r of
        Right (_ :: [L Int]) -> return ()
        Left e -> assertFailure $ show e
  , testCase "Nullable parser, error" $ do
      let r = tokensEither (longestToken decimal <> badWhitespace) "-" "31? 45"
      case r of
        Right (_ :: [L Int]) -> assertFailure "No error?"
        Left (LexicalError p) -> p @?= Pos "-" 1 3 2
  , testCase "No matches, error" $ do
      tokensEither (longestToken decimal) "-" " " @?= Left (LexicalError (Pos "-" 1 1 0))
  , testCase "No matches after a recognized token" $ do
      fmap unloc (runLexer (longestToken decimal <> ws) "-" "2 x") @?=
        TsToken (2 :: Int, Loc (Pos "-" 1 1 0) (Pos "-" 1 1 0)) (TsError $ LexicalError (Pos "-" 1 3 2))
  , testCase "longestShortest (success)" $
      fmap (map unLoc)
      (tokensEither ((Left <$> blockComment) <> (Right <$> word) <> ws)
        "-"
        "/* xxx */ yyy /*** abc ***/ ef")
      @?=
        Right [Left ("/*"," xxx */"),Right "yyy",Left ("/***"," abc ***/"),Right "ef"]
  ]

-- orphan
instance NFData a => NFData (L a) where
  rnf (L loc a) = loc `seq` rnf a
