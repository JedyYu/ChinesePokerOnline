--该模块定义的一些基本的类型，洗牌发牌的函数，和判断能否出牌的函数
module Cards where
import System.IO.Unsafe
import System.Random
import Data.List

data Suit = Spades | Hearts | Diamonds | Clubs | Jokers
    deriving (Ord, Eq, Enum)

instance Show Suit where
    show Spades = "s"
    show Hearts = "h"
    show Diamonds = "d"
    show Clubs = "c"
    show Jokers = "j"

data Value = Three | Four | Five | Six | Seven | Eight | Nine 
             | Ten | Jack | Queen | King | Ace | Two | Jol | Job
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

--player
data Player = Player
  { name      :: [Char]
  , pockets   :: [Card]
  , lastround :: [Card]
  , numCard   :: Int
  , identity  :: Identity 
  } deriving (Eq, Show)

--玩家身份，农民或地主
data Identity = Farmer | Landlord 
  deriving (Eq, Show)

--Game中保存了整个牌局的信息
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

--deck
type Deck = [Card]

--生成全部的54张牌
fullDeck :: Deck
fullDeck = [Card v s | v <-[Three .. Two], s <-[Spades,Hearts,Diamonds,Clubs]]
           ++[Card v s | v <-[Jol,Job], s <-[Jokers]]

--洗牌函数
shuffleDeck :: Deck -> Deck
shuffleDeck xs = parse_deck $ shuffle' xs (length xs)
    where 
        shuffle' _ 0    = return []
        shuffle' xs len = 
            do  n           <- randomRIO (0, len - 1)
                let (y, ys) =  choose n xs
                ys'         <- shuffle' ys (len - 1)
                return (y:ys')
        choose _ []     = error "choose: index out of range"
        choose 0 (x:xs) = (x, xs)
        choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)
        parse_deck :: IO Deck -> Deck
        parse_deck io = unsafePerformIO io

--发n张牌
dealNCards :: Int -> Deck -> [Card]
dealNCards n xxs = sort $ take n xxs

--from hands
type Hand = [Card]
data Combo = Single | Couple | Triple | Bomb | Rocket | SingleChain | DoubleChain | TripleChain | ThreewithOne | ThreewithTwo | FourwithTwo | Plane 
    deriving (Show, Eq)
--分别是        单牌|     双牌| 三张牌|  炸弹|    火箭|       单顺子|       双顺子|       三顺子|        三带一|        三带二|       四带二| 飞机带翅膀

extraction :: [Card] -> [Value]
extraction a  = map (value ) a
----------------------------------------------------------------------
--判断是否为火箭
isRocket :: Hand -> Bool
isRocket a
        | length(a) == 2 && sort(a)==[Card Jol Jokers , Card Job Jokers] = True
        | otherwise = False
----------------------------------------------------------------------
--判断是否为单牌
isSingle :: Hand -> Bool
isSingle a = length(a) == 1

--判断两张单牌能不能出，a是前一轮出牌，b是此轮出牌
geSingle :: Hand -> Hand -> Bool
geSingle a b
        | isSingle a && isSingle b && a < b = True
        | otherwise = False
----------------------------------------------------------------------
--判断是否为双牌
isCouple :: Hand -> Bool
isCouple a
        | length(a) == 2 && value(head(a)) == value(last(a)) = True
        | otherwise = False

--判断两张双牌能不能出，a是前一轮出牌，b是此轮出牌
geCouple :: Hand -> Hand -> Bool
geCouple a b
        | isCouple a && isCouple b && head(a) < head(b) = True
        | otherwise = False
----------------------------------------------------------------------
--判断是否为三张牌
isTriple :: Hand -> Bool
isTriple a
        | isCouple(taill(a)) && value(head(a))==value(last(a)) = True
        | otherwise = False

geTriple :: Hand -> Hand -> Bool
geTriple a b
        | isTriple a && isTriple b && head(a) < head(b) = True
        | otherwise = False
----------------------------------------------------------------------
--判断是否为炸弹
isBomb :: Hand -> Bool
isBomb a
        | isTriple(taill(a)) && value(head(a))==value(last(a)) = True
        | otherwise = False

geBomb :: Hand -> Hand -> Bool
geBomb a b 
        | isBomb a && isBomb b && head(a) < head(b) = True
        | otherwise = False
----------------------------------------------------------------------
isSingleChain :: Hand -> Bool
isSingleChain a
        | length(a) >= 5 && ( isSubsequenceOf (sort(extraction(a))) [ Three .. Ace ] ) = True
        | otherwise = False

geSingleChain :: Hand -> Hand -> Bool
geSingleChain a b 
        | isSingleChain a && isSingleChain b && length(a)==length(b) && head(a) < head(b) = True
        | otherwise = False
----------------------------------------------------------------------
isDoubleChain :: Hand -> Bool
isDoubleChain a
        | length b >= 6 &&   ( isSubsequenceOf (group b) c ) = True
        | otherwise = False
        where b = sort ( extraction (a) )
              c = group $ sort( [ Three .. Ace ]++[ Three .. Ace ] )


geDoubleChain :: Hand -> Hand -> Bool
geDoubleChain a b 
        | isDoubleChain a && isDoubleChain b && length(a)==length(b) && head(a) < head(b) = True
        | otherwise = False              
----------------------------------------------------------------------
isTripleChain :: Hand -> Bool
isTripleChain a
        | length b >= 6 &&   ( isSubsequenceOf (group b) c ) = True
        | otherwise = False
        where b = sort ( extraction (a) )
              c = group $ sort( [ Three .. Ace ]++[ Three .. Ace ]++[ Three .. Ace ] )

geTripleChain :: Hand -> Hand -> Bool
geTripleChain a b 
        | isTripleChain a && isTripleChain b && length(a)==length(b) && head(a) < head(b) = True
        | otherwise = False        
----------------------------------------------------------------------
isThreewithOne :: Hand -> Bool
isThreewithOne a = ( (isSubsequenceOf (initt b) d && isSubsequenceOf (taill b ) c ) && (largemember a /= smallmember a) )|| (((isSubsequenceOf (initt b ) c) && isSubsequenceOf (taill b ) d )  && (largemember a /= smallmember a) )
        where b = group ( sort ( extraction (a) ) )
              c = group $ sort( [ Three .. Ace ]++[ Three .. Ace ]++[ Three .. Ace ] )
              d = group [Three .. Two ]

geThreewithOne :: Hand -> Hand -> Bool
geThreewithOne a b = ((isThreewithOne a)&&(isThreewithOne b)&&(largemember a < largemember b))

--获得三带一中三张牌的值
largemember :: Hand -> Value
largemember a 
         | length (head b )== 3 =  (head (head b ))
         | otherwise = (head (last b))
         where b = group ( sort ( extraction (a) ) )  

--获得三张牌中单牌的值
smallmember :: Hand -> Value
smallmember a 
         | length (head b )== 3 =  (head (last b ))
         | otherwise = (head (head b))
         where b = group ( sort ( extraction (a) ) )  
----------------------------------------------------------------------
isThreewithTwo :: Hand -> Bool
isThreewithTwo a = ((length a == 5) && (isSubsequenceOf (initt b) d && isSubsequenceOf (taill b ) c ) || ((length a == 5) && (isSubsequenceOf (initt b ) c) && isSubsequenceOf (taill b ) d ) )
        where b = group ( sort ( extraction (a) ) )
              c = group $ sort( [ Three .. Ace ]++[ Three .. Ace ]++[ Three .. Ace ] )
              d = group $ sort ([Three .. Two ]++[ Three .. Ace ] )

geThreewithTwo :: Hand -> Hand -> Bool
geThreewithTwo a b = ((isThreewithTwo a)&&(isThreewithTwo b)&&(largemember a < largemember b))

----------------------------------------------------------------------
isFourwithTwo :: Hand -> Bool
isFourwithTwo a = (length a == 6)&&(((length b) == 2) && ((isSubsequenceOf (initt b) c)||(isSubsequenceOf (taill b) c)) || ((length b) == 3) && ((isSubsequenceOf (initt (initt b)) c) || (isSubsequenceOf (initt (taill b)) c) || (isSubsequenceOf (taill (taill b)) c)))
        where b = group ( sort ( extraction (a) ) )
              c = group $ sort( [ Three .. Ace ]++[ Three .. Ace ]++[ Three .. Ace ]++[ Three .. Ace ]  )

geFourwithTwo :: Hand -> Hand -> Bool
geFourwithTwo a b = (isFourwithTwo a) && (isFourwithTwo b) && (largemember4 a1 < largemember4 b1)
        where a1 = group ( sort ( extraction (a) ) )
              b1 = group ( sort ( extraction (b) ) )

--获得四带一中四张牌的值
largemember4 :: [[Value]] -> Value
largemember4 a
         | length (head a) == 4 =  (head (head a))
         | otherwise = largemember4 (taill a)
----------------------------------------------------------------------
isPlane :: Hand -> Bool
isPlane a = ( isTripleChain f ) && ((length c == length d) || (length c == length e))
      where b = groupBy ( groupbyvalue ) (sort a)
            c = [cad | cad <- b , length cad == 3]
            d = [dad | dad <- b, length dad == 1]
            e = [ead | ead <- b , length ead ==2]
            f = foldr (++) [] c

groupbyvalue :: Card -> Card -> Bool
groupbyvalue a b = (value a == value b)

gePlane :: Hand -> Hand -> Bool
gePlane a b = (isPlane a) && (isPlane b) && (geTripleChain aaa1 bbb1)
      where a1 = groupBy ( groupbyvalue ) (sort a)
            aa1 = [cad | cad <- a1 , length cad == 3]
            aaa1 = foldr (++) [] aa1
            b1 = groupBy ( groupbyvalue ) (sort b)
            bb1 = [cad | cad <- b1 , length cad == 3]
            bbb1 = foldr (++) [] bb1
----------------------------------------------------------------------
--根据上轮的出牌判断此轮的出牌是否合法
validPlay :: Hand -> Hand -> Bool
validPlay a b 
        | b==[] = False
        | a==[] = test b
        | isRocket b = True
        | geSingle a b = True
        | geCouple a b = True
        | geTriple a b = True
        | geBomb a b = True
        | isBomb b = True
        | geTripleChain a b = True
        | geDoubleChain a b = True
        | geSingleChain a b = True
        | geThreewithTwo a b = True
        | geThreewithOne a b = True
        | geFourwithTwo a b = True
        | gePlane a b = True
        | otherwise = False
        where test b = isSingle b || isCouple b || isTriple b || isBomb b || isRocket b || isSingleChain b || isDoubleChain b || isTripleChain b || isThreewithOne b || isThreewithTwo b || isPlane b || isFourwithTwo b 

--将字符串格式的牌转换成Card类型的牌
changeFormat :: [[Char]] -> [Card]
changeFormat a = foldr (++) [] (map read1 a)

--根据前两轮的出牌，判断此轮玩家 要跟上轮的牌 还是跟上上轮的牌
validPlay1 :: [[Char]] -> [[Char]] -> [[Char]] -> Bool
validPlay1 a b c
    | a==[] && b==[] = validPlay [] (changeFormat c)
    | b==[] && a/=[] = validPlay (changeFormat a) (changeFormat c)
    | b/=[] = validPlay (changeFormat b) (changeFormat c)
