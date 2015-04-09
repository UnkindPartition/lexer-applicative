{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.HUnit

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char
import Data.Loc (L(..), Loc(..), Pos(..))
import Control.Exception
import Control.DeepSeq

whitespace = () <$ some (psym isSpace)

unloc (L l a) = (a, l)

-- this is bad, because it accepts an empty string
badWhitespace = () <$ many (psym isSpace)

main = defaultMain $ testGroup "Tests"
  [ testCase "Empty string" $
      tokens (empty :: RE Char Int) empty "-" "" @=? []
  , testCase "Space- and newline-separated numbers" $
      unloc <$> tokens decimal whitespace "-" "1\n 23  456" @?=
      [ (1,  Loc (Pos "-" 1 1 0) (Pos "-" 1 1 0))
      , (23, Loc (Pos "-" 2 2 3) (Pos "-" 2 3 4))
      , (456,Loc (Pos "-" 2 6 7) (Pos "-" 2 8 9))
      ]
  , testCase "Nullable parser, no error" $ do
      r <- try . evaluate $ tokens decimal badWhitespace "-" "31 45"
      case r of
        Right (_ :: [L Int]) -> return ()
        Left (e :: SomeException) -> assertFailure $ show e
  , testCase "Nullable parser, error" $ do
      r <- try . evaluate . force $ tokens decimal badWhitespace "-" "31? 45"
      case r of
        Right (_ :: [L Int]) -> assertFailure "No error?"
        Left (LexicalError p) -> p @?= Pos "-" 1 3 2
  ]

-- orphan
instance NFData a => NFData (L a) where
  rnf (L loc a) = loc `seq` rnf a
