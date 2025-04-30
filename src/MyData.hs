module MyData where


newtype Name = Name String deriving (Show, Read, Eq)
newtype Price = Price Float deriving (Show, Read, Eq)
newtype Category = Category String deriving (Show, Read, Eq)

data Item = Item {
    itemName     :: Name,
    itemPrice    :: Price,
    itemCategory :: Category
} deriving (Show, Read, Eq)

-- Корзина
data Cart = Cart {
    cartItems :: [(Item, Int)]
} deriving (Show, Read)

-- Бонусная карта
data BonusCard = BonusCard {
    birthDate :: (Int, Int, Int),  -- (год, месяц, день)
    basediscount  :: Float  -- скидка до 7%
} deriving (Show, Read)