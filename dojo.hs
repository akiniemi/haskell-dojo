--
-- Copyright (C) 2012 Aki Niemi
--
-- This is public domain. Use as you wish, but hold harmless the
-- author.
--

import Data.List

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Int } | Joker
    deriving (Show, Eq, Ord)

data Hand =
    RoyalFlush
  | StraightFlush
  | FourOfKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfKind
  | TwoPairs
  | Pair
  | HighCard
  deriving (Eq, Show)

checkHand :: [Card] -> Hand
checkHand [] = error "Empty hand"
checkHand cards@(x:xs)
  | count /= 5                       = error "Wrong number of cards"
  | royal && flush                   = RoyalFlush
  | straight && flush                = StraightFlush
  | 4 `elem` kinds                   = FourOfKind
  | 3 `elem` kinds && 2 `elem` kinds = FullHouse
  | flush                            = Flush
  | straight                         = Straight
  | 3 `elem` kinds                   = ThreeOfKind
  | length (filter (==2) kinds) == 2 = TwoPairs
  | 2 `elem` kinds                   = Pair
  | otherwise                        = HighCard
  where count    = length cards
        kinds    = map length $ group ranks
        flush    = all (\each -> suit each == suit x) xs
        ranks    = sort . map rank $ cards
        straight = ranks `isInfixOf` [1..14]
        royal    = ranks == [10..14] || ranks == 1:[10..13]

testHighCard      = [Card Hearts 14, Card Hearts 2, Card Spades 11, Card Diamonds 12, Card Spades 5]
testPair          = [Card Hearts 14, Card Spades 14, Card Diamonds 3, Card Hearts 4, Card Clubs 7]
testTwoPairs      = [Card Hearts 14, Card Spades 14, Card Diamonds 3, Card Hearts 7, Card Clubs 7]
testThreeOfKind   = [Card Hearts 14, Card Spades 14, Card Diamonds 14, Card Hearts 4, Card Clubs 7]
testStraight      = [Card Hearts 3, Card Spades 2, Card Diamonds 5, Card Hearts 4, Card Clubs 6]
testStraightAce   = [Card Hearts 3, Card Spades 2, Card Diamonds 5, Card Hearts 4, Card Clubs 1]
testFlush         = [Card Hearts 14, Card Hearts 4, Card Hearts 3, Card Hearts 8, Card Hearts 7]
testFlushJoker    = [Joker, Card Hearts 4, Card Hearts 3, Card Hearts 8, Card Hearts 7]
testFullHouse     = [Card Hearts 14, Card Spades 14, Card Diamonds 3, Card Hearts 3, Card Clubs 3]
testFourOfKind    = [Card Hearts 14, Card Spades 14, Card Diamonds 14, Card Hearts 4, Card Clubs 14]
testStraightFlush = [Card Hearts 11, Card Hearts 9, Card Hearts 7, Card Hearts 8, Card Hearts 10]
testRoyalFlush    = [Card Hearts 11, Card Hearts 13, Card Hearts 12, Card Hearts 10, Card Hearts 14]
testRoyalFlushAce = [Card Hearts 11, Card Hearts 13, Card Hearts 12, Card Hearts 10, Card Hearts 1]

tests = [
  (HighCard, testHighCard), (Pair, testPair), (TwoPairs, testTwoPairs), (ThreeOfKind, testThreeOfKind),
  (Straight, testStraight), (Straight, testStraightAce), (Flush, testFlush), (Flush, testFlushJoker),
  (FullHouse, testFullHouse), (FourOfKind, testFourOfKind), (StraightFlush, testStraightFlush),
  (RoyalFlush, testRoyalFlush), (RoyalFlush, testRoyalFlushAce)
  ]

run :: [(Hand,[Card])] -> String
run suite
  | and [ checkHand (snd t) == fst t | t <- suite ] = "Passed"
  | otherwise = "Failed"
