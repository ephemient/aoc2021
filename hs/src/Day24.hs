{-|
Module:         Day24
Description:    <https://adventofcode.com/2021/day/24 Day 24: Arithmetic Logic Unit>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day24 (day24a, day24b) where

import Control.Monad.Primitive (PrimMonad)
import Data.Bits ((.&.))
import Data.Hashable (Hashable(hashWithSalt))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Vector.Mutable as MV (exchange, replicate)
import Data.Void (Void)
import System.Random (randomIO)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), choice, chunk, eof, parse, sepEndBy, single)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Register = W | X | Y | Z

data Instruction a b
  = Inp a
  | Add a (Either a b)
  | Mul a (Either a b)
  | Div a (Either a b)
  | Mod a (Either a b)
  | Eql a (Either a b)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m (Instruction Register a)
parser = choice
  [ Inp <$> (chunk "inp " *> reg)
  , Add <$> (chunk "add " *> reg) <*> (single ' ' *> value)
  , Mul <$> (chunk "mul " *> reg) <*> (single ' ' *> value)
  , Div <$> (chunk "div " *> reg) <*> (single ' ' *> value)
  , Mod <$> (chunk "mod " *> reg) <*> (single ' ' *> value)
  , Eql <$> (chunk "eql " *> reg) <*> (single ' ' *> value)
  ] where
    reg = choice [W <$ single 'w', X <$ single 'x', Y <$ single 'y', Z <$ single 'z']
    value = (Left <$> reg) <|> (Right <$> signed (pure ()) decimal)

day24 :: (PrimMonad m, Eq a, Hashable a, Integral a) => Text -> [a] -> Int -> m (Either (ParseErrorBundle Text Void) (Maybe a))
day24 input nums salt = case parse (parser `sepEndBy` newline <* eof) "" input of
    Left e -> pure $ Left e
    Right ins -> Right <$> do
        cache <- MV.replicate 0x1000000 Nothing
        let go prefix (_, _, _, 0) [] = pure $ Just prefix
            go _ (_, _, _, _) [] = pure Nothing
            go prefix r ((i, Inp a):ins') = do
                let state = (i, r)
                    go' num cont =
                        go (10 * prefix + num) (set r a num) ins' >>= maybe cont (pure . Just)
                seen <- MV.exchange cache (hashWithSalt salt state .&. 0xffffff) $ Just state
                if seen == Just state then pure Nothing else foldr go' (pure Nothing) nums
            go prefix r ((_, Add a b):ins') =
                go prefix (set r a $ get r a + either (get r) id b) ins'
            go prefix r ((_, Mul a b):ins') =
                go prefix (set r a $ get r a * either (get r) id b) ins'
            go prefix r ((_, Div a b):ins') =
                go prefix (set r a $ get r a `div` either (get r) id b) ins'
            go prefix r ((_, Mod a b):ins') =
                go prefix (set r a $ get r a `mod` either (get r) id b) ins'
            go prefix r ((_, Eql a b):ins') =
                go prefix (set r a $ if get r a == either (get r) id b then 1 else 0) ins'
        go 0 (0, 0, 0, 0) $ zip [0 :: Int ..] ins
  where
    get (w, _, _, _) W= w
    get (_, x, _, _) X= x
    get (_, _, y, _) Y= y
    get (_, _, _, z) Z= z
    set (_, x, y, z) W w = (w, x, y, z)
    set (w, _, y, z) X x = (w, x, y, z)
    set (w, x, _, z) Y y = (w, x, y, z)
    set (w, x, y, _) Z z = (w, x, y, z)

day24a :: Text -> IO (Either (ParseErrorBundle Text Void) (Maybe Int))
day24a input = day24 input [9, 8..1] =<< randomIO

day24b :: Text -> IO (Either (ParseErrorBundle Text Void) (Maybe Int))
day24b input = day24 input [1..9] =<< randomIO
