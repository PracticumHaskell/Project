module MyData ( 
    Name(..)
  , Price(..)
  , Category(..)
  , Item(..)
  , Cart(..)
  , BonusCard(..)
  )
where


newtype Name = Name String deriving (Show, Read, Eq)
newtype Price = Price Float deriving (Show, Read, Eq)
newtype Category = Category String deriving (Show, Read, Eq)


data Item = Item {
    itemName     :: Name,
    itemPrice    :: Price,
    itemCategory :: Category
} deriving (Show, Read, Eq)

-- Cart
data Cart = Cart {
    cartItems :: [(Item, Int)]
} deriving (Show, Read)

-- Bonus card
data BonusCard = BonusCard {
    birthDate :: (Int, Int, Int), --(year,month,date)
    basediscount  :: Float  -- discount not more than 7%
} deriving (Show, Read)