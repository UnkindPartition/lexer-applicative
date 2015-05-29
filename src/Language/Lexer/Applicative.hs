{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}
-- | For some background, see
-- <https://ro-che.info/articles/2015-01-02-lexical-analysis>
module Language.Lexer.Applicative
  (
    -- * Building a Lexer
    Lexer(..)
  , token
  , whitespace
    -- ** Building Recognizers
  , Recognizer
  , longest
  , longestShortest
    -- * Running a Lexer
  , runLexer
    -- ** Working with a token stream
  , TokenStream(..)
  , streamToList
  , streamToEitherList
  , LexicalError(..)
  ) where

import Text.Regex.Applicative
import Data.Loc
import Data.List
import Data.Typeable (Typeable)
import Data.Monoid
import Data.Function
import Control.Exception
import GHC.Exts

----------------------------------------------------------------------
--                             Lexer
----------------------------------------------------------------------

-- | A 'Lexer' specification consists of two recognizers: one for
-- meaningful tokens and one for whitespace and comments.
--
-- Although you can construct 'Lexer's directly, it is more convenient to
-- build them with 'token', 'whitespace', and the 'Monoid' instance like this:
--
-- @
--  myLexer :: 'Lexer' MyToken
--  myLexer = 'mconcat'
--    [ 'token'      ('longest' myToken)
--    , 'whitespace' ('longest' myWhiteSpace)
--    , 'whitespace' ('longestShortest' myComment)
--    ]
-- @
data Lexer tok = Lexer
  { lexerTokenRE :: Recognizer tok
  , lexerWhitespaceRE :: Recognizer ()
  }

instance Monoid (Lexer tok) where
  mempty = Lexer mempty mempty
  Lexer t1 w1 `mappend` Lexer t2 w2 = Lexer (t1 <> t2) (w1 <> w2)

-- | Build a lexer with the given token recognizer and no (i.e. 'mempty')
-- whitespace recognizer.
--
-- 'token' is a monoid homomorphism:
--
-- @'token' a '<>' 'token' b = 'token' (a '<>' b)@
token :: Recognizer tok -> Lexer tok
token r = Lexer r mempty

-- | Build a lexer with the given whitespace recognizer and no (i.e. 'mempty')
-- token recognizer.
--
-- 'whitespace' is a monoid homomorphism:
--
-- @'whitespace' a '<>' 'whitespace' b = 'whitespace' (a '<>' b)@
whitespace :: Recognizer a -> Lexer tok
whitespace r = Lexer mempty (() <$ r)

----------------------------------------------------------------------
--                           Recognizer
----------------------------------------------------------------------

-- | A token recognizer
--
-- 'Recognizer' values are constructed by functions like 'longest' and
-- 'longestShortest', combined with `mappend`, and used by 'token' and
-- 'whitespace'.
--
-- When a recognizer returns without consuming any characters, a lexical
-- error is signaled.
newtype Recognizer tok = Recognizer (RE Char (RE Char tok))
  deriving Functor

instance Monoid (Recognizer tok) where
  mempty = Recognizer empty
  mappend (Recognizer r1) (Recognizer r2) = Recognizer (r1 <|> r2)

-- | When scanning a next token, the regular expression will compete with
-- the other 'Recognizer's of its 'Lexer'. If it wins, its result
-- will become the next token.
--
-- 'longest' has the following properties:
--
-- * @'longest' (r1 '<|>' r2) = 'longest' r1 '<>' 'longest' r2@
--
-- * @'longest' r = 'longestShortest' 'const' r ('const' '$' 'pure' ())@
longest
  :: RE Char tok
  -> Recognizer tok
longest re = longestShortest const re (const $ pure ())

-- | This is a more sophisticated recognizer than 'longest'.
--
-- It recognizes a token consisting of a prefix and a suffix, where prefix
-- is chosen longest, and suffix is chosen shortest.
--
-- An example would be a C block comment
--
-- >/* comment text */
--
-- The naive
--
-- @'longest' ('string' "\/*" '*>' 'many' 'anySym' '*>' 'string' "*\/")@
--
-- doesn't work because it consumes too much: in
--
-- >/* xxx */ yyy /* zzz */
--
-- it will treat the whole line as a comment.
--
-- This is where 'longestShortest' comes in handy:
--
-- @
-- 'longestShortest'
--    (\\_ _ -> ()) -- don't care about the comment text
--    ('string' "\/*")
--    (\\_ -> 'many' 'anySym' '*>' 'string' "*\/")
-- @
--
-- Operationally, the prefix regex first competes with other 'Recognizer's
-- for the longest match. If it wins, then the shortest match for the
-- suffix regex is found, and the two results are combined with the given
-- function to produce a token.
--
-- The two regular expressions combined must consume some input, or else
-- 'LexicalError' is thrown. However, any one of them may return without
-- consuming input.
--
-- \* * *
--
-- Once the prefix regex wins, the choice is committed; the suffix regex
-- must match or else a 'LexicalError' is thrown. Therefore,
--
-- @
-- 'longestShortest' f pref suff1
--          '<>'
-- 'longestShortest' f pref suff2
--          =
-- 'longestShortest' f pref suff1
-- @
--
-- and is not the same as
--
-- @'longestShortest' f pref (suff1 '<|>' suff2)@
--
-- The following holds, however:
--
-- @
-- 'longestShortest' f pref1 suff
--          '<>'
-- 'longestShortest' f pref2 suff
--          =
-- 'longestShortest' f (pref1 '<|>' pref2) suff
-- @
--
-- \* * *
--
-- Passing the result of prefix into both suffix and combining function may
-- seem superfluous; indeed we could get away with
--
-- @'RE' 'Char' pref -> (pref -> 'RE' 'Char' tok) -> 'Recognizer' tok@
--
-- or even
--
-- @'RE' 'Char' ('RE' 'Char' tok) -> 'Recognizer' tok@
--
-- This is done purely for convenience and readability; the intention is
-- that @pref@ passed into suffix is used to customize the regular
-- expression which would still return only its part of the token, and then
-- the function will combine the two parts. Of course, you don't need to
-- follow this recommendation. Thanks to parametricity, all three versions
-- are equivalent.
longestShortest
  :: (pref -> suff -> tok)
  -> RE Char pref -- ^ regex for the longest prefix
  -> (pref -> RE Char suff) -- ^ regex for the shortest suffix
  -> Recognizer tok
longestShortest f prefRE suffRE =
  Recognizer $
    (\pref -> f pref <$> suffRE pref) <$> prefRE

----------------------------------------------------------------------
--                           Running a Lexer
----------------------------------------------------------------------

-- | The lexical error exception
data LexicalError = LexicalError !Pos
  deriving (Eq, Typeable)

instance Show LexicalError where
  show (LexicalError pos) = "Lexical error at " ++ displayPos pos
instance Exception LexicalError

-- | A stream of tokens
data TokenStream tok
  = TsToken tok (TokenStream tok)
  | TsEof
  | TsError LexicalError
  deriving (Functor, Show)

instance IsList (TokenStream tok) where
  type Item (TokenStream tok) = tok
  toList = streamToList
  fromList = foldr TsToken TsEof

-- | Convert a 'TokenStream' to a list of tokens. Turn 'TsError' into
-- a runtime 'LexicalError' exception.
streamToList :: TokenStream tok -> [tok]
streamToList stream =
  case stream of
    TsToken t stream' -> t : streamToList stream'
    TsEof -> []
    TsError e -> throw e

-- | Convert a 'TokenStream' into either a token list or a 'LexicalError'.
-- This function may be occasionally useful, but in general its use is
-- discouraged because it needs to force the whole stream before returning
-- a result.
streamToEitherList :: TokenStream tok -> Either LexicalError [tok]
streamToEitherList =
  sequence .
  fix (\rec stream ->
    case stream of
      TsToken t stream' -> Right t : rec stream'
      TsEof -> []
      TsError e -> [Left e]
  )

-- | Run a lexer on a string and produce a lazy stream of tokens
runLexer
  :: forall tok.
     Lexer tok -- ^ lexer specification
  -> String -- ^ source file name (used in locations)
  -> String -- ^ source text
  -> TokenStream (L tok)
runLexer (Lexer (Recognizer pToken) (Recognizer pJunk)) src = go . annotate src
  where
  go l = case l of
    [] -> TsEof
    s@((_, pos1, _):_) ->
      case findLongestPrefix re s of

        Nothing -> TsError (LexicalError pos1)

        Just (shortest_re, rest1) ->

          case findShortestPrefix shortest_re rest1 of
            -- If the combined match is empty, we have a lexical error
            Just (v, (_, pos1', _):_) | pos1' == pos1 ->
              TsError $ LexicalError pos1

            Just (Just tok, rest) ->
              let
                pos2 =
                  case rest of
                    (_, _, p):_ -> p
                    [] -> case last s of (_, p, _) -> p

              in TsToken (L (Loc pos1 pos2) tok) (go rest)

            Just (Nothing, rest) -> go rest

  extend :: RE Char a -> RE (Char, Pos, Pos) a
  extend = comap (\(c, _, _) -> c)

  re :: RE (Char, Pos, Pos) (RE (Char, Pos, Pos) (Maybe tok))
  re = extend . fmap extend $
    ((Just <$>) <$> pToken) <|> ((Nothing <$) <$> pJunk)

annotate
  :: String -- ^ source file name
  -> String -- ^ contents
  -> [(Char, Pos, Pos)] -- ^ the character, its position, and the previous position
annotate src s = snd $ mapAccumL f (startPos src, startPos src) s
  where
    f (pos, prev_pos) ch =
      let pos' = advancePos pos ch
      in pos' `seq` ((pos', pos), (ch, pos, prev_pos))
