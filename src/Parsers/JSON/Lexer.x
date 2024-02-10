{
module Parsers.JSON.Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  ) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*
tokens :-
<0> $white+ ;
{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }
get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)
put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())
modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())
alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  pure $ RangedToken EOF (Range pos pos)
data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)
data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)
data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Logical operators
  | And
  | Or
  -- Parenthesis
  | LPar
  | RPar
  -- Lists
  | Comma
  | LBrack
  | RBrack
  -- Types
  | Colon
  | Arrow
  -- EOF
  | EOF
  deriving (Eq, Show)
}
