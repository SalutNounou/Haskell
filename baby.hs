import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map      as Map
import qualified Data.Set      as Set

removeLowerCase xs = [x | x<- xs, x `elem` ['A' .. 'Z']]

factorial n = product [1..n]



lucky :: (Integral a) => a->String
lucky 7 = "Bravo, tu as gagne!"
lucky a = "Desole, tu n'as pas de chance..."


sayMe:: (Integral a) => a->String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3= "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe a = "Not between one and five"


factorial' :: (Integral a) => a-> a
factorial' 0 = 1
factorial' a = a * factorial' (a-1)

addVectors:: (Num a)=> (a,a)-> (a,a)->(a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors'::(Num a)=>(a,a)->(a,a)->(a,a)
addVectors' (x,y) (u,v) = (x+u, y+v)

head':: [a]->a
head' []=error "Can't return head of an empty list, dummmy!"
head' (x:xs)=x



calcBmis :: (RealFloat a)=>[(a,a)]->[a]
calcBmis xs = [ bmi w h | (w,h) <- xs]
         where bmi weight height = weight/ height^2


bmiTell :: (RealFloat a)=> a->a->String
bmiTell weight height
    | bmi <= skinny ="You're underweight"
    | bmi <=regular = "You're alright"
    | bmi <=fat = "You're overweight"
    | otherwise ="You're a fat bastard"
    where bmi = weight/ height^2
          (skinny,regular,fat) = (18.5,25,30)



max' :: (Ord a)=>a->a->a
max' a b
     |a>b =a
     | otherwise = b


compare' ::(Ord a)=>a->a-> Ordering
compare' x y
       |x<y =LT
       |x>y = GT
       |otherwise = EQ


initials :: String->String->String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
       where (f:_) = firstname
             (l:_) = lastname


cylinder :: (RealFloat a)=> a->a->a
cylinder h r =
	     let base = pi * r^2
	         side = 2* pi * r
	     in side + 2 * base


myLast :: [a]->a
myLast [] = error "the list is empty!"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast ::[a]->a
myButLast [] = error "The list is empty"
myButLast [x] = error "The list is a singleton"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

myLength :: [a]->Int
myLength xs = sum [1| _<-xs]


myReverse :: [a]->[a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) =  myReverse xs  ++ [x]

isPalindrome :: (Eq a)=> [a]->Bool
isPalindrome [] = True
isPalindrome x = x == myReverse x

myReverse' ::[a]->[a]
myReverse' [] = []
myReverse' (x:xs) = myReverse'' (x:xs) []
         where myReverse'' [] reversed = reversed
               myReverse'' (b:c) reversed = myReverse'' c (b:reversed)

myLargestDivisible::(Integral a)=>a
myLargestDivisible =head( filter isDivisible [100000,99999..])
     where isDivisible x = x `mod` 3829 == 0

sumOddSquares::(Integral a)=>a
sumOddSquares = sum(takeWhile (<10000)(filter odd (map (^2) [1..])))

chaine::(Integral a)=>a->[a]
chaine 1 = [1]
chaine x
	| odd x = x : chaine(3 * x + 1)
	| otherwise = x : chaine (x `div` 2)

longChains::Int
longChains = length (filter isLongChain (map chaine [1..100]))
    where isLongChain x = length x >15

sumFold::(Num a)=>[a]->a
sumFold x = foldl (\acc y -> acc + y) 0 x

elemFold::(Eq a)=>a->[a]->Bool
elemFold x xs= foldl(\acc y -> acc || x==y ) False xs

mapFold::(a->b)->[a]->[b]
mapFold f x = foldr (\y acc-> (f y ):acc) [] x

maximumFold::(Ord a)=>[a]->a
maximumFold x = foldl1(\acc y-> if(y>acc) then y else acc) x

reverseFold::[a]->[a]
reverseFold x = foldl (\acc y-> y : acc)[] x

productFold::(Num a)=>[a]->a
productFold = foldl1(\acc x -> acc * x)

filterFold::(a->Bool)->[a]->[a]
filterFold f  = foldr(\y acc-> if(f y ) then y:acc else acc)[]

headFold::[a]->a
headFold = foldr1(\x acc-> x)

lastFold::[a]->a
lastFold = foldl1(\acc x->x)

search::(Eq a)=>[a]->[a]->Bool
search  x xs = let xlen = length x in foldl (\acc y-> (take xlen y == x )||acc) False (tails xs)

myWords::String->[String]
myWords  = filter (not . any isSpace) . groupBy((==) `on` isSpace)


myEncode::Int->String->String
myEncode offSet = map ( chr . (+offSet) . ord)

myDecode::Int->String->String
myDecode offSet = myEncode (negate offSet)


findKey::(Eq a)=>a->[(a,b)]->b
findKey key  = snd . head . filter(\(k,v)->key==k)


findKey'::(Eq a)=>a->[(a,b)]->Maybe b
findKey' _ [] = Nothing
findKey' key ((k,v):xs) = if k==key then Just v else findKey' key xs


findKey''::(Eq a)=>a->[(a,b)]->Maybe b
findKey'' key = foldl(\acc (k,v)-> if k==key then Just v else acc) Nothing

createMyMap =Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]

myFromList::(Ord k)=>[(k,v)]->Map.Map k v
myFromList  = foldr(\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap::(Ord k)=>[(k,a)]->Map.Map k [a]
phoneBookToMap  = Map.fromListWith (++) . map(\ (x,y) -> (x,[y]))

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

phoneBook =
        [("betty","555-2938")
        ,("betty","342-2492")
        ,("bonnie","452-2928")
        ,("patsy","493-2928")
        ,("patsy","943-2929")
        ,("patsy","827-9162")
        ,("lucille","205-2928")
        ,("wendy","939-8282")
        ,("penny","853-2492")
        ,("penny","555-2111")
        ]
