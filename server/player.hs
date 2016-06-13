module Player where

import Cards
import Deck
--import Hands

data Player = Player
  { name      :: [Char]
  , pockets   :: [Card]
  , lastround :: [Card]
  , numCard   :: Int
  , identity  :: Identity 
  } deriving (Eq, Show)

data Identity = Farmer | Landlord | Others 
  deriving (Eq, Show)

--type WinnerID = Maybe Identity

data Game = Game
  { players   :: [Player]
  , last1round :: [Card]
  , last2round :: [Card]
  , last3round :: [Card]
  , holeCard  :: [Card]
  , turn      :: [Char]
  , roundNum  :: Int
  , lordFlag  :: Int
  , lordName  :: [Char]
  , multi     :: Int
  }

