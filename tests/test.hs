{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.HUnit

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char
import Data.Monoid
import Data.Loc (L(..), Loc(..), Pos(..))
import Control.Exception
import Control.DeepSeq

ws = whitespace $ longest $ some (psym isSpace)
-- this is bad, because it accepts an empty string
badWhitespace = whitespace $ longest $ many (psym isSpace)

longestToken = token . longest

unloc (L l a) = (a, l)


main = defaultMain $ testGroup "Tests"
  [ testGroup "tokens"
    [ testCase "Empty string" $
        tokens (longestToken (empty :: RE Char Int) <> whitespace mempty) "-" "" @=? []
    , testCase "Space- and newline-separated numbers" $
        unloc <$> tokens (longestToken decimal <> ws) "-" "1\n 23  456" @?=
        [ (1,  Loc (Pos "-" 1 1 0) (Pos "-" 1 1 0))
        , (23, Loc (Pos "-" 2 2 3) (Pos "-" 2 3 4))
        , (456,Loc (Pos "-" 2 6 7) (Pos "-" 2 8 9))
        ]
    , testCase "Nullable parser, no error" $ do
        r <- try . evaluate $ tokens (longestToken decimal <> badWhitespace) "-" "31 45"
        case r of
          Right (_ :: [L Int]) -> return ()
          Left (e :: SomeException) -> assertFailure $ show e
    , testCase "Nullable parser, error" $ do
        r <- try . evaluate . force $ tokens (longestToken decimal <> badWhitespace) "-" "31? 45"
        case r of
          Right (_ :: [L Int]) -> assertFailure "No error?"
          Left (LexicalError p) -> p @?= Pos "-" 1 3 2
    ]
    -- end testGroup "tokens"

  , testGroup "tokensEither"
    [ testCase "Returns Right upon success" $ do
        tokensEither (longestToken decimal <> ws) "-" "1" @=? Right [L (Loc (Pos "-" 1 1 0) (Pos "" 1 1 0)) 1]
    , testCase "Returns Left upon failure" $ do
        tokensEither (longestToken decimal <> ws) "-" "a" @=? Left (LexicalError (Pos "-" 1 1 0))
    ]
  ]

-- orphan
instance NFData a => NFData (L a) where
  rnf (L loc a) = loc `seq` rnf a
