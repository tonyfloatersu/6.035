-- Scanner -- Decaf scanner                                     -*- haskell -*-
-- Copyright (C) 2013  Benjamin Barenblat <bbaren@mit.edu>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.
{
{-# OPTIONS_GHC -w #-}
module Scanner ( ScannedToken(..)
               , Token(..)
               , scan
               , formatTokenOrError
               ) where

import Data.Word (Word8)
import Text.Printf (printf)
import Data.Char (ord)

import qualified Data.Bits

}

----------------------------------- Tokens ------------------------------------

$alpha = [a-zA-Z]

tokens :-
  $white+ ;
  "//".*  ;                     -- comment
  class   { \posn s -> scannedToken posn $ Keyword s }
  \{      { \posn _ -> scannedToken posn LCurly }
  \}      { \posn _ -> scannedToken posn RCurly }
  $alpha+ { \posn s -> scannedToken posn $ Identifier s }


----------------------------- Representing tokens -----------------------------

{
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

-- | Hard-encode the width of a tab as 8. 
-- | This will affect the column number information 
-- | whenever tab is involved.
tabW :: Int
tabW = 8

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+tabW-1) `div` tabW)*tabW+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Word8],       -- pending bytes on current char
                  String)

-- | Yields the next byte of present char, or the first byte of next char.
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

-- | Recall previous char.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

-- | A token with position information.
data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , extractRawToken :: Token
                                 } deriving (Eq)

-- | A token.
data Token = Keyword String
           | Identifier String
           | LCurly
           | RCurly
           deriving (Eq)
instance Show Token where
  show (Keyword k) = k
  show (Identifier s) = "IDENTIFIER " ++ s
  show LCurly = "{"
  show RCurly = "}"

{-| Smart constructor to create a 'ScannedToken' by extracting the line and
column numbers from an 'AlexPosn'. -}
scannedToken :: AlexPosn -> Token -> ScannedToken
scannedToken (AlexPn _ lineNo columnNo) tok = ScannedToken lineNo columnNo tok


---------------------------- Scanning entry point -----------------------------

scan :: String -> [Either String ScannedToken]
scan str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn pos line column), prevChar, currentBytes, inpString) ->
                  let badChar = head inpString in
                  let errorMessage = printf "line %d:%d: unexpected char: '%c'" line column badChar :: String
                      inp' = ( AlexPn (pos + 1) line (column + 1)
                             , prevChar
                             , currentBytes
                             , drop 1 inpString
                             ) in
                  Left errorMessage : go inp'
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> Right (act pos (take len str)) : go inp'

formatTokenOrError :: Either String ScannedToken -> Either String String
formatTokenOrError (Left err) = Left err
formatTokenOrError (Right tok) = Right $ unwords [ show $ line tok
                                                 , show $ extractRawToken tok
                                                 ]
}
