{
{-# LANGUAGE DeriveFoldable #-}
module Parsers.JSON.Parser
  ( parseJSON
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Parsers.JSON.Lexer as L
}

%name parseJSON
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%expect 0

%token
  -- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (L.String _) _ }
  integer    { L.RangedToken (L.Integer _) _ }
  -- Objects
  '{'        { L.RangedToken L.LCurly _ }
  '}'        { L.RangedToken L.RCurly _ }
  -- Lists
  '['        { L.RangedToken L.LSquare _ }
  ']'        { L.RangedToken L.RSquare _ }
  ','        { L.RangedToken L.Comma _ }
  -- Types
  ':'        { L.RangedToken L.Colon _ }

%%

Value : '{'  '}' { Object }
      | '['  ']' { Array [] }
      | string   { unTok $1 (\range (L.String string) -> String string) }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2

-- * AST

data Value
  = Object
  | Array [Value]
  | String ByteString
  | Number Float
  | Boolean Bool
  | Null
  deriving (Show)

}
