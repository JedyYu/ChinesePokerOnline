module Cards where

data Suit = Spades | Hearts | Diamonds | Clubs | Jokers
    deriving (Ord, Eq, Enum)

instance Show Suit where
    show Spades = "s"
    show Hearts = "h"
    show Diamonds = "d"
    show Clubs = "c"
    show Jokers = "j"

data Value = Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace | Two | Jol | Job
    deriving (Eq, Ord, Enum)

instance Show Value where 
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "X"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"
    show Jol = "L"
    show Job = "B"

data Card = Card {value :: Value, suit :: Suit}
    deriving (Eq)

--instance Read Card where
--    read a = read1 a
taill :: (Eq a)=> [a] -> [a]
taill b
     |b==[] = []
     |otherwise = tail b

initt :: (Eq a)=> [a] -> [a]
initt b
     |b==[] = []
     |otherwise = init b

read1 :: [Char] -> [Card]
read1 a 
    | a == [] = [] 
    | otherwise = explain (fst b) : read1 (taill (snd b))
       where b = break (==',') a

explain :: [Char] -> Card
explain a= Card (explainV (head a)) (explainS (last a))
    where explainV a
                  |a=='2'=Two
                  |a=='3'=Three
                  |a=='4'=Four
                  |a=='5'=Five
                  |a=='6'=Six
                  |a=='7'=Seven
                  |a=='8'=Eight
                  |a=='9'=Nine
                  |a=='X'=Ten
                  |a=='J'=Jack
                  |a=='Q'=Queen
                  |a=='K'=King
                  |a=='A'=Ace
                  |a=='L'=Jol
                  |a=='B'=Job
                  |otherwise=error "choose: index out of range"
          explainS a
                  |a=='s' = Spades 
                  |a=='h' =Hearts
                  |a=='d' =Diamonds
                  |a=='c' =Clubs
                  |a=='j' =Jokers
                  |otherwise=error "choose: index out of range"

instance Show Card where
    show (Card value suit) = show value ++ show suit

instance Ord Card where
    a > b = value a > value b
    a < b = value a < value b
    a <= b = value a < value b ||  value a == value b
    a >= b = value a > value b ||  value a == value b
