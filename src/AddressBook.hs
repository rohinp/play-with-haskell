{-# LANGUAGE NamedFieldPuns #-}
module AddressBook where

import Control.Applicative (liftA3)

-- Lets do some validation using Applicative

--Step one Lets first create some data to work on
data Address = Address {
      street :: String
    , city :: String
    , state :: String
    } deriving (Show, Eq)

-- a helper function to create a record
address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

-- a function to validate the field
empty:: String -> Maybe String
empty "" = Nothing
empty xs = Just xs

{-
Now this wont work
address (empty strt) (empty cty) (empty stat)
obviously the address does not expects a Maybe

Step 2.
Functor and Applicative to the rescure
so this is what we can do
-}

validateAddress1 :: Address -> Maybe Address
validateAddress1 Address{street, city, state} = address <$> empty street <*> empty city <*> empty state

--There are many methods to achive this, lets do one more
validateAddress2 ::Address -> Maybe Address
validateAddress2 Address{street, city, state} = liftA3 address (empty street) (empty city) (empty state)

--lets introduce a bit variation in validation and also some error messages
emptyEither :: String -> Either [String] ()
emptyEither str = if null str then Left ["Empty string error"] else Right ()

spaceEither :: String -> Either [String] ()
spaceEither str = if ' ' `elem` str then Left ["Space in string error"] else Right ()

-- using the newly created validations on address
validateAddress3 :: Address -> Either [String] Address
validateAddress3 Address{street, city, state} = address <$> 
  (emptyEither street *> pure street) <*>
  (emptyEither city *> spaceEither city *> pure city) <*>
  pure state
  
