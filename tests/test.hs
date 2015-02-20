import Test.Tasty
import Test.Tasty.HUnit

import Language.Lexer.Applicative
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char
import Data.Loc (L(..), Loc(..), Pos(..))

whitespace = () <$ some (psym isSpace)

unloc (L l a) = (a, l)

main = defaultMain $ testGroup "Tests"
  [ testCase "Empty string" $
      tokens (empty :: RE Char Int) empty "-" "" @=? []
  , testCase "Space- and newline-separated numbers" $
      unloc <$> tokens decimal whitespace "-" "1\n 23  456" @=?
      [ (1,  Loc (Pos "-" 1 0 0) (Pos "-" 1 0 0))
      , (23, Loc (Pos "-" 2 1 3) (Pos "-" 2 2 4))
      , (456,Loc (Pos "-" 2 5 7) (Pos "-" 2 7 9))
      ]
  ]
