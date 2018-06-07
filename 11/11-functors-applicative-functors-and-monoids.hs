import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show, Eq)

-- Make the list a Functor
instance Functor List where
    fmap _ Empty = Empty
    fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Empty = a
combineLists (Value x xs) b = Value x (combineLists xs b) 

-- Make our list a Monoid

instance Semigroup (List a) where
    (<>) = combineLists

instance Monoid (List a) where
    mempty = Empty
    mappend = combineLists 

-- Make our list an Applicative

instance Applicative List where
    pure a = Value a Empty
    (<*>) Empty _ = Empty
    (<*>) (Value f fs) something = mappend (fmap f something) ((<*>) fs something)  

-- Make sure that the List obeys the laws for Applicative and Monoid

x = (Value 1 Empty)
y = (Value 2 Empty)
z = (Value 3 Empty)
xyz = [x,y,z]

identity = x <> mempty == mempty <> x
associative = x <> y <> z == x <> (y <> z)
catamorphism = mconcat xyz == foldr (<>) mempty [x, y, z]

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

test1 = plusTwo <$> twoValueList
test2 = plusTwo <$> plusTwo <$> twoValueList

-- Use <$> and <*> on the lists with a binary function

test3 = pure (*) <*> plusTwo <$> twoValueList <*> twoValueList  

-- Create some lists of binary functions

-- Use <*> on the binary functions list and the number lists
