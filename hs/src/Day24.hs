{-|
Module:         Day24
Description:    <https://adventofcode.com/2021/day/24 Day 24: Arithmetic Logic Unit>
-}
{-# LANGUAGE DeriveFunctor, FlexibleContexts, NamedFieldPuns, OverloadedStrings, TypeFamilies #-}
module Day24 (day24) where

import Control.Monad (join)
import Data.Maybe (catMaybes, listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
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
  deriving (Functor)

data State a = State { w :: !a, x :: !a, y :: !a, z :: !a } deriving (Functor)

get :: State a -> Register -> a
get State { w } W = w
get State { x } X = x
get State { y } Y = y
get State { z } Z = z

set :: State a -> Register -> a -> State a
set state W w' = state { w = w' }
set state X x' = state { x = x' }
set state Y y' = state { y = y' }
set state Z z' = state { z = z' }

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

run :: (Integral a) => [a] -> a -> [Instruction Register a] -> State a -> Maybe a
run nums prefix = run' where
    run' [] State { z = 0 } = Just prefix
    run' [] _ = Nothing
    run' (ins:rest) state
      | Inp a <- ins
      , runRange (minimum nums, maximum nums) (fmap (join (,)) <$> (ins:rest)) $ join (,) <$> state
      = listToMaybe $ catMaybes [run nums (10 * prefix + i) rest $ set state a i | i <- nums]
      | Inp _ <- ins = Nothing
      | Add a b <- ins = run' rest $ set state a $ get state a + either (get state) id b
      | Mul a b <- ins = run' rest $ set state a $ get state a * either (get state) id b
      | Div a b <- ins = run' rest $ set state a $ get state a `div` either (get state) id b
      | Mod a b <- ins = run' rest $ set state a $ get state a `mod` either (get state) id b
      | Eql a b <- ins = run' rest $ set state a $ get state a `eql` either (get state) id b
      where a `eql` b = if a == b then 1 else 0

runRange :: (Integral a) => (a, a) -> [Instruction Register (a, a)] -> State (a, a) -> Bool
runRange r ins0 state0 = runRange' ins0 state0 /= Just False where
    runRange' [] State { z = (0, 0) } = Just True
    runRange' [] State { z = (z0, z1) } = Just $ z0 <= 0 && 0 <= z1
    runRange' (ins:rest) state
      | Inp a <- ins = runRange' rest (set state a r)
      | Add a b <- ins = runRange' rest . set state a =<< get state a +: either (get state) id b
      | Mul a b <- ins = runRange' rest . set state a =<< get state a *: either (get state) id b
      | Div a b <- ins = runRange' rest . set state a =<< get state a /: either (get state) id b
      | Mod a b <- ins = runRange' rest . set state a =<< get state a %: either (get state) id b
      | Eql a b <- ins = runRange' rest . set state a =<< get state a =: either (get state) id b
    (a, b) +: (c, d) = Just (a + c, b + d)
    (a, b) *: (c, d)
      | a >= 0 && c >= 0 = Just (a * c, b * d)
      | b <= 0 && d <= 0 = Just (b * d, a * c)
      | a >= 0 && d <= 0 = Just (a * d, b * c)
      | b <= 0 && c >= 0 = Just (b * c, a * d)
      | xs <- [0, a * c, a * d, b * c, b * d] = Just (minimum xs, maximum xs)
    (a, b) /: (c, d)
      | c > 0 = Just (a `div` d, b `div` c)
      | d < 0 = Just (a `div` c, b `div` d)
      | otherwise = Nothing
    (a, b) %: (c, d)
      | c > 0 && c == d = Just $ if b - a + 1 < c && a `mod` c <= b `mod` c
        then (a `mod` c, b `mod` c)
        else (0, c - 1)
      | otherwise = Nothing
    (a, b) =: (c, d)
      | a == b && b == c && c == d = Just (1, 1)
      | a <= d && c <= b = Just (0, 1)
      | otherwise = Just (0, 0)

day24 :: Text -> Either (ParseErrorBundle Text Void) (Maybe (Integer, Integer))
day24 input = do
    ins <- parse (parser `sepEndBy` newline <* eof) "" input
    Right $ (,) <$> run [9, 8..1] 0 ins (State 0 0 0 0) <*> run [1..9] 0 ins (State 0 0 0 0)

{-
data Expr a
  = Literal a
  | Expr a :+ Expr a
  | Expr a :* Expr a
  | Expr a :/ Expr a
  | Expr a :% Expr a
  | Expr a := Expr a
  deriving (Eq, Ord)

instance (Show a) => Show (Expr a) where
    showsPrec n (Literal a) = showsPrec n a
    showsPrec n e@(lhs :+ rhs)
      | n <= 6 = showsPrec 6 lhs . (" + " ++) . showsPrec 6 rhs
      | otherwise = ('(':) . showsPrec 6 e . (')':)
    showsPrec n e@(lhs :* rhs)
      | n <= 7 = showsPrec 7 lhs . (" * " ++) . showsPrec 7 rhs
      | otherwise = ('(':) . showsPrec 7 e . (')':)
    showsPrec n e@(lhs :/ rhs)
      | n <= 7 = showsPrec 7 lhs . (" / " ++) . showsPrec 8 rhs
      | otherwise = ('(':) . showsPrec 7 e . (')':)
    showsPrec n e@(lhs :% rhs)
      | n <= 7 = showsPrec 7 lhs . (" % " ++) . showsPrec 8 rhs
      | otherwise = ('(':) . showsPrec 7 e . (')':)
    showsPrec n e@(lhs := rhs)
      | n <= 4 = showsPrec 5 lhs . (" = " ++) . showsPrec 5 rhs
      | otherwise = ('(':) . showsPrec 4 e . (')':)

executeInstruction :: [Expr a] -> State (Expr a) -> Instruction Register a -> ([Expr a], State (Expr a))
executeInstruction inputs state input
  | Inp a <- input = (tail inputs, set a $ head inputs)
  | Add a b <- input = (inputs, set a $ get a :+ either get Literal b)
  | Mul a b <- input = (inputs, set a $ get a :* either get Literal b)
  | Div a b <- input = (inputs, set a $ get a :/ either get Literal b)
  | Mod a b <- input = (inputs, set a $ get a :% either get Literal b)
  | Eql a b <- input = (inputs, set a $ get a := either get Literal b)
  where
    get W = w state
    get X = x state
    get Y = y state
    get Z = z state
    set W w' = state { w = w' }
    set X x' = state { x = x' }
    set Y y' = state { y = y' }
    set Z z' = state { z = z' }

values :: (Ord a, Show a) => Expr (Either a Int) -> IntMap (Set (Map a IntSet))
values (Literal (Left a)) = IntMap.fromDistinctAscList
    [(x, Set.singleton $ Map.singleton a $ IntSet.singleton x) | x <- [1..9]]
values (Literal (Right b)) = IntMap.singleton b $ Set.singleton Map.empty
values (x :+ Literal (Right 0)) = values x
values (x :+ Literal (Right n)) = IntMap.mapKeysMonotonic (+ n) $ values x
values (x :+ y) = IntMap.fromListWith (∪)
    [(u + v, a ∩ b) | (u, a) <- IntMap.assocs $ values x, (v, b) <- IntMap.assocs $ values y]
values (_ :* Literal (Right 0)) = IntMap.singleton 0 $ Set.singleton Map.empty
values (x :* Literal (Right n))
  | n > 0 = IntMap.mapKeysMonotonic (* n) $ values x
  | otherwise = IntMap.mapKeys (* n) $ values x
values (x :* y) = IntMap.fromListWith (∪)
    [(u * v, a ∩ b) | (u, a) <- IntMap.assocs $ values x, (v, b) <- IntMap.assocs $ values y]
values (x :/ Literal (Right 1)) = values x
values (x :/ y) = IntMap.fromListWith (∪)
    [(u `div` v, a ∩ b) | (u, a) <- IntMap.assocs $ values x, (v, b) <- IntMap.assocs $ values y]
values (x :% y) = IntMap.fromListWith (∪)
    [(u `mod` v, a ∩ b) | (u, a) <- IntMap.assocs $ values x, (v, b) <- IntMap.assocs $ values y]
values (x := Literal (Right 0)) = IntMap.fromDistinctAscList
  [ (1, foldr (∪) Set.empty $ IntMap.elems below ++ IntMap.elems above)
  , (1, fromMaybe Set.empty zero)
  ] where
    (below, zero, above) = IntMap.splitLookup 0 $ values x
values (x := y) = IntMap.fromListWith (∪)
    [(u =? v, a ∩ b) | (u, a) <- IntMap.assocs $ values x, (v, b) <- IntMap.assocs $ values y]
  where u =? v = if u == v then 1 else 0

(∪), (∩) :: (Ord a) => Set (Map a IntSet) -> Set (Map a IntSet) -> Set (Map a IntSet)
lhs ∪ rhs = Set.fromList $ map (Map.filter (/= IntSet.fromDistinctAscList [1..9])) $
    Map.unionWith IntSet.union <$> Set.elems lhs <*> Set.elems rhs
lhs ∩ rhs = Set.fromList $ filter (not . any IntSet.null) $
    Map.unionWith IntSet.intersection <$> Set.elems lhs <*> Set.elems rhs

day24' :: Text -> Either (ParseErrorBundle Text Void) (Maybe (Int, Int))
day24' input = do
    ins <- parse (parser `sepEndBy` newline <* eof) "" input
    let zero = Literal $ Right 0
        (_, State { z }) = foldl' (uncurry executeInstruction)
            (Literal . Left <$> ['A'..], State zero zero zero zero) $
            fmap Right <$> ins
        n = length [() | Inp _ <- ins]
        m0 = Map.fromDistinctAscList $ take n $ (, IntSet.fromDistinctAscList [1, 9]) <$> ['A'..]
        f a b = 10 * a + b
        g a b = if IntSet.null a then b else a
        maximize m = foldl' f 0 $ IntSet.findMax <$> Map.elems (Map.unionWith g m m0)
        minimize m = foldl' f 0 $ IntSet.findMin <$> Map.elems (Map.unionWith g m m0)
    pure $ do
        constraints <- values z IntMap.!? 0
        traceShowM constraints
        (getMin *** getMax) . mconcat . map (Max . maximize &&& Min . minimize) <$>
            nonEmpty (Set.elems constraints)
-}
