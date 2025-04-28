module Lib
  ( cartTotal
  , calculateDiscount
  , printReceiptIO
  , readItemFile
  , makeItemData
  , checkItemLines
  , readCartFile
  , checkCartLines
  , makeCartData
  , filteredContent
  , printWarnings2
  , runProgram
  ) where


import Data.Time.Clock
import Data.Time.Calendar
import System.Environment ()
import Data.List.Split


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

-- стоимость корзины без скидки
cartTotal :: Cart -> Float
cartTotal (Cart items) = sum [ getPrice item * fromIntegral qty |(item, qty) <- items ]
  where
    getPrice (Item _ (Price p) _) = p

-- Расчет скидки:день рождения, сумма заказа большая и категория фрукты
calculateDiscount :: Cart -> BonusCard -> IO Float
calculateDiscount cart (BonusCard birth baseDiscount) = do
    currentTime <- getCurrentTime
    let (_, month, day) = toGregorian (utctDay currentTime)
        isBirthday = month == snd3 birth && day == thd3 birth
        total = cartTotal cart
        extraDiscount = if total > 1000 then 2 else if total > 500 then 1 else 0
        categoryBonus = if any (isCategory (Category "Фрукты")) (cartItems cart) then 1 else 0
        finalDiscount = min 7 (baseDiscount + extraDiscount + categoryBonus)

    return $ if isBirthday then min 7 finalDiscount else finalDiscount
  where
    snd3 (_, x, _) = x
    thd3 (_, _, y) = y
    isCategory cat (Item _ _ c, _) = c == cat

-- Вывод чека 
printReceiptIO :: Cart -> BonusCard -> IO ()
printReceiptIO cart@(Cart items) card = do
    let total = cartTotal cart
    discount <- calculateDiscount cart card
    let discountAmount = total * (discount / 100)
    let finalTotal = total - discountAmount
    putStrLn "Чек:"
    putStrLn "----------------------------------------"
    mapM_ printItem items
    putStrLn "----------------------------------------"
    putStrLn $ "Итого без скидки: " ++ show total ++ " руб."
    putStrLn $ "Скидка: " ++ show discountAmount ++ " руб. (" ++ show discount ++ "%)"
    putStrLn $ "Итого со скидкой: " ++ show finalTotal ++ " руб."
  where
    printItem (Item (Name n) (Price p) _, q) =
        putStrLn $ n ++ " - " ++ 
        show q ++ " шт. - " ++ 
        show p ++ " руб. за ед. - " ++ 
        show (p * fromIntegral q) ++ " руб."


readItemFile :: FilePath -> IO (Either String [Item])
readItemFile path = do
    content <- readFile path 
    putStrLn content
    case checkItemLines content of
        Left err -> return $ Left err
        Right v -> do 
                let content1 = (chunksOf 3  v)
                let con = makeItemData content1
                return $ Right con


checkItemLines :: String -> Either String [String]
checkItemLines content = 
    case all validItemLine (filter (not . null) (lines content)) of
        True -> Right (words content)
        False -> Left "Error format of items file: line has to be: name price(Float) category"
    where 
        validItemLine line = 
                    case words line of
                    [name, price, categ] -> not (null name) && isFloat price && not (null categ)
                    _ -> False
        isFloat s = case reads s :: [(Float, String)] of
                    [(_, "")] -> True
                    _         -> False 

makeItemData :: [[String]] -> [Item]
makeItemData [] = []
makeItemData ([a, b, c] : cs) =
  (Item { itemName = Name a
        , itemPrice = Price (read b)
        , itemCategory = Category c
        }) : makeItemData cs
makeItemData (_ : cs) = makeItemData cs


readCartFile :: FilePath -> [Item] -> IO (Either String Cart)
readCartFile path items = do
    content <- readFile path 
    putStrLn content
    case checkCartLines content of --проверка формата
        Left err -> return $ Left err
        Right c -> do
                let content1 = (chunksOf 2  c)
                printWarnings2 content1 items -- если товаров из корзины нету в items
                let filCont = filteredContent content1 items--если нету каких-то товаров убираем
                let con = Cart (makeCartData filCont items)
                return $ Right con

checkCartLines :: String -> Either String [String]
checkCartLines content =
    case all validLine (lines content) of
        True -> Right (words content)
        False -> Left "Error cart format: correct line format: name quantity(Int)"
    where
        validLine line =
                    case words line of
                    [name, qStr] -> not (null name) && isInt qStr
                    _ -> False
        isInt q = case reads q :: [(Int, String)] of
                    [(_, "")] -> True
                    _ -> False

makeCartData :: [[String]] -> [Item] -> [(Item, Int)]
makeCartData [] _ = []
makeCartData ([name, q] : cs) it = (getItem name it, read q) : makeCartData cs it
  where
    getItem n ((i@(Item (Name nam) _ _): its))| n == nam = i
                                              | otherwise = getItem n its
    getItem _ [] = Item (Name "") (Price 2) (Category "")
makeCartData (_: cs) it = makeCartData cs it

filteredContent :: [[String]] -> [Item] -> [[String]]
filteredContent [] _ = []
filteredContent ((c : cs) : cont) it
  | c `elem` [ ni | (Name ni) <- (map itemName it)] =
      (c : cs) : filteredContent cont it
  | otherwise =
      filteredContent cont it
filteredContent (_ : cont) it = filteredContent cont it


printWarnings2 :: [[String]] -> [Item] -> IO ()
printWarnings2 cartRaw items = do
    let cartNames = [x | (x : _) <- cartRaw]
        itemNames = [n | Item (Name n) _ _ <- items]
        unknowns  = filter (`notElem` itemNames) cartNames
    case unknowns of
        [] -> return () 
        _  -> putStrLn $ "These items are not in store and will be deleted: " ++ 
                        unwords unknowns

readBonusFile :: FilePath -> IO (Either String BonusCard)
readBonusFile path = do
    content <- readFile path
    putStrLn $ content ++ "%"
    case checkBonusLines content of
        Left err -> return $ Left err
        Right (y, m, d, discount) -> do
            let card = BonusCard (y, m, d) discount
            return $ Right card

checkBonusLines :: String -> Either String (Int, Int, Int, Float)
checkBonusLines content =
     case filter (not . null) (lines content) of
        [dateL, discountL] ->
            case splitOn "." dateL of
                [y, m, d]
                  | all isInt [y, m, d] && isFloat discountL ->
                      Right (read y, read m, read d, read discountL)
                  | otherwise -> Left "Error format: has to be: date(Int.Int.Int) or number(Float)"
                _ -> Left "Date has to be in form Year.Month.day(2000.4.20)"
        _ -> Left "File has to consist of 2 lines: date and discount"
  where
    isInt s = case reads s :: [(Int, String)] of
        [(_, "")] -> True
        _ -> False

    isFloat s = case reads s :: [(Float, String)] of
        [(_, "")] -> True
        _ -> False

runProgram :: FilePath -> FilePath -> Maybe FilePath -> IO ()
runProgram itemsFile cartFile bonusFileM = do
    eItems <- readItemFile itemsFile
    case eItems of
        Left err -> putStrLn $ "Error reading items list: " ++ err
        Right items -> do
            if any (\(Item _ (Price p) _) -> p < 0) items
                then putStrLn "Error: in items list are items with negative price"
                else do
                    eCart <- readCartFile cartFile items
                    case eCart of
                        Left err -> putStrLn $ "Erroe reading cart: " ++ err
                        Right (Cart cartList) -> do
                            let getCard = maybe (return $ Right (BonusCard (2000, 1, 1) 0)) 
                                        readBonusFile bonusFileM
                            eCard <- getCard
                            case eCard of
                                Left err -> putStrLn $ "Error reading bonus file: " ++ err
                                Right (BonusCard birth discount)
                                    | discount < 0 || discount > 7 ->
                                        putStrLn "Error: bonus card discount has to be from 0 to 7"
                                    | any (\(_, q) -> q < 0) cartList ->
                                        putStrLn "Error: in cart are items with negative quantit"
                                    | otherwise -> do
                                        let finalCart = Cart cartList
                                        printReceiptIO finalCart (BonusCard birth discount)
