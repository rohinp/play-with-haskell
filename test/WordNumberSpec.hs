module WordNumberSpec(spec) where

import Test.Hspec
import Data.List (intersperse, sort)
import Test.QuickCheck

numbersInString :: [String]
numbersInString = [ "zero"
                , "one"
                , "two"
                , "three"
                , "four"
                , "five"
                , "six"
                , "seven"
                , "eight"
                , "nine"
                ]
digitToWord :: Int -> String
digitToWord n = numbersInString !! n

digits :: Int -> [Int]
digits n
    | n == 0    = []
    | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber = joinWords . getWords
  where
    joinWords  = concat . intersperse "-"
    getWords n = map digitToWord $ digits n

numberGen :: Gen Int
numberGen = elements ([0..9]::[Int])

prop_shuldAlwaysreturnNonEmptyString :: Property
prop_shuldAlwaysreturnNonEmptyString = 
    forAll numberGen
        (\i -> (length $ digitToWord i) > 0)

oneTo4DigitNumGen :: Gen Int
oneTo4DigitNumGen = elements [1..10000]

prop_shouldMatchDigitsLength :: Property
prop_shouldMatchDigitsLength = 
    forAll oneTo4DigitNumGen
        (\i -> (length $ concat $ map show $ digits i) ==  (length $ show i))


spec :: Spec
spec = do 
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
        it "always returns a string" $ do
            prop_shuldAlwaysreturnNonEmptyString
    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]
        it "digit length and return value length must match" $ do
            prop_shouldMatchDigitsLength
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    describe "using QuickCheck" $ do
        it "half identity" $ do 
            prop_halfIdentity
        it "check if list if ordered" $ do
            prop_ifListIsSorted
        it "Associtivity for plus" $ do
            prop_associativeForPlus
        it "Cummutative plus" $ do
            prop_commutativeForPlus
        it "Associtivity for Mul" $ do
            prop_associativeForMul
        it "Cummutative Mul" $ do
            prop_commutativeForMul



-- using quick check
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

floatGen :: Gen Float
floatGen = arbitrary

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll floatGen (\x -> x == halfIdentity x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

listGen :: Gen [Float]
listGen = arbitrary

prop_ifListIsSorted :: Property
prop_ifListIsSorted =
  forAll listGen (\xs ->  (listOrdered . sort $ xs) == True)

associativeOperation :: (Eq a, Num a) => (a -> a -> a) -> a -> a -> a -> Bool
associativeOperation op x y z = x `op` (y `op` z) == (x `op` y) `op` z

commutativeOperation :: (Eq a, Num a) => (a -> a -> a) -> a -> a -> Bool
commutativeOperation op x y = x `op` y == y `op` x

triplets :: Gen (Int, Int, Int)
triplets = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

tuples :: Gen (Int, Int)
tuples = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

prop_associativeForPlus :: Property
prop_associativeForPlus =
  forAll triplets (\(x, y, z) -> associativeOperation (+) x y z)

prop_commutativeForPlus :: Property
prop_commutativeForPlus =
  forAll tuples (\(x, y) -> commutativeOperation (+) x y)

prop_associativeForMul :: Property
prop_associativeForMul =
  forAll triplets (\(x, y, z) -> associativeOperation (*) x y z)

prop_commutativeForMul :: Property
prop_commutativeForMul =
  forAll tuples (\(x, y) -> commutativeOperation (*) x y)

integerArbitraryWithoutZero :: Gen Integer
integerArbitraryWithoutZero = arbitrary `suchThat` (/= 0)

twoNonZeroes :: Gen (Integer, Integer)
twoNonZeroes = do
  a <- integerArbitraryWithoutZero
  b <- integerArbitraryWithoutZero
  return (a, b)

quotRemEq :: Integral a => a -> a -> Bool
quotRemEq x y =
  (quot x y) * y + (rem x y) == x

divModEq :: Integral a => a -> a -> Bool
divModEq x y =
  (div x y) * y + (mod x y) == x

prop_quotRemEq :: Property
prop_quotRemEq =
  forAll twoNonZeroes (\(x, y) -> quotRemEq x y)

prop_divModEq :: Property
prop_divModEq =
  forAll twoNonZeroes (\(x, y) -> divModEq x y)

-- power

positeIntegerArbitrary = arbitrary `suchThat` (> 1)

threeNonNegatives :: Gen (Integer, Integer, Integer)
threeNonNegatives = do
  a <- positeIntegerArbitrary
  b <- positeIntegerArbitrary
  c <- positeIntegerArbitrary
  return (a, b, c)

-- quickCheck verifies this is not true
--powerAssociative x y z =
-- x ^ (y ^ z) == (x ^ y) ^ z

-- also not true
--powerCommutative x y =
--  x ^ y == y ^ x

prop_powerAssociative :: Property
prop_powerAssociative =
  forAll threeNonNegatives (\(x, y, z) -> associativeOperation (^) x y z)

prop_powerCommutative :: Property
prop_powerCommutative =
  forAll twoNonZeroes (\(x, y) -> commutativeOperation (^) x y)

-- reverseId

prop_reverseId :: Property
prop_reverseId =
  forAll (arbitrary :: Gen [String])
  (\x -> (reverse . reverse $ x) == id x)


-- prop $

prop_dollar :: Property
prop_dollar =
  forAll (arbitrary :: Gen Int)
  (\x -> id $ x == id x)

prop_compose :: Property
prop_compose =
  forAll (arbitrary :: Gen Int)
  (\x -> id . id $ x == (id (id x)))

-- fold cons concat

prop_foldCons :: Property
prop_foldCons =
  forAll (arbitrary :: Gen [Int])
  (\x -> (foldr (:) [] x) == ((++) [] x))

prop_foldConcat :: Property
prop_foldConcat =
  forAll (arbitrary :: Gen [[Int]])
  (\x -> (foldr (++) [] x) == (concat x))

-- hm is that so

genTuple :: Gen (Int, [Int])
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

hmm :: Int -> [a] -> Int
hmm n xs = length (take n xs)

-- proves is not true
prop_hmm :: Property
prop_hmm =
  forAll (genTuple :: Gen (Int, [Int]))
  (\(x, xs) -> (hmm x xs) == x)

-- round trip
roundTrip :: (Read a1, Show a2) => a2 -> a1
roundTrip x = (read (show x))

prop_roundTrip :: Property
prop_roundTrip =
  forAll (arbitrary :: Gen Int)
  (\x -> roundTrip x == x)

trivialInt :: Gen Int
trivialInt = return 1

randomToOneHundred :: Gen Int
randomToOneHundred = elements [1..100]

genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple' :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple' = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))
            ]