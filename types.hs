module Main (main) where

import System.IO
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

data Value = S {getString :: String}
           | I Int 
           | D Double 
           | Null 
           | B Bool 
           | O {getObject :: JObject} 
           | A [Value]
   deriving (Show)

data JObject = Object { getStringObjects :: [Property] } deriving (Show)

data Property = StringObject { propertyString :: String, propertyValue :: Value } deriving (Show)

-- Terminal & Non terminal
valueP :: Parser Value
valueP = tagP "cardinal" @> (\(_,n,_) -> I (read n))
   <|> tagP "scard" @> (\(_,n,_) -> I (read n))
   <|> tagP "decimal" @> (\(_,n,_) -> D (read n))
   <|> tagP "string" @> (\(_,n,_) -> S ( trimQuotes n ))
   <|> tagP "null" @> (\(_,_,_) -> Null)
   <|> tagP "true" @> (\(_,_,_) -> B True)
   <|> tagP "false" @> (\(_,_,_) -> B False)
   <|> tagP "exp" @> (\(_,l,_) -> D (expToDouble l))
   <|> arrayP
   <|> objectP

-- Terminal only
valueOnlyP :: Parser Value
valueOnlyP = tagP "cardinal" @> (\(_,n,_) -> I (read n))
   <|> tagP "scard" @> (\(_,n,_) -> I (read n))
   <|> tagP "decimal" @> (\(_,n,_) -> D (read n))
   <|> tagP "string" @> (\(_,n,_) -> S ( trimQuotes n ))
   <|> tagP "null" @> (\(_,_,_) -> Null)
   <|> tagP "true" @> (\(_,_,_) -> B True)
   <|> tagP "false" @> (\(_,_,_) -> B False)
   <|> tagP "exp" @> (\(_,l,_) -> D (expToDouble l))

-- substring after 'e' or 'E'
postE :: Bool -> String -> String
postE _ [] = []
postE b [x]
   | b = [x]
postE b (x:xs)
   | x == 'e' || x == 'E' = postE True xs
   | b = x : postE b xs
   | not b = postE b xs

-- substring before 'e' or 'E'
preE :: String -> String
preE [] = []
preE [x]
   | x /= 'e' && x /= 'E' = [x]
   | otherwise = []
preE (x:xs)
   | x /= 'e' && x /= 'E' = x : preE xs
   | otherwise = []

-- Exponents are always double to prevent any potential rounding error when trying to determine if int.
-- Get first half, get second half, do the math
expToDouble :: String -> Double
expToDouble s = (read pre) * (10 ** (read post))
   where
      pre = preE s
      post = postE False s

-- removes redundant quotes from the start and end of strings
trimQuotes :: String -> String
trimQuotes n = drop 1 $ take (length n - 1) n

objectP :: Parser Value
objectP = arrayP <|> (tagP "{"
      <&> many (propertyP <& (optional (tagP ",")))
      <& tagP "}"
      @> (\(_, e) -> O (Object e))) <|> valueOnlyP
-- accomodates for single value JSONs too

arrayP :: Parser Value
arrayP = tagP "["
       &> many (valueP <& (optional (tagP ",")))
      <& tagP "]"
      @> (\(values) -> A values)


propertyP :: Parser Property
propertyP = tagP "string"
      <&> literalP ":" ":"
       &> nofail' "value expected" valueP
      @> (\((_,n,_), e) -> StringObject {propertyString = (trimQuotes n), propertyValue = e})


decimalL :: Lexer
decimalL = (signedCardinalL <|> cardinalL)
         <&&> literalL '.'
         <&&> cardinalL


signedDecimalL :: Lexer
signedDecimalL = literalL '-'
         <&&> (signedCardinalL <|> cardinalL)
         <&&> literalL '.'
         <&&> cardinalL

valueL :: Lexer
valueL = tagFilter "null" $ dropWhite $ nofail $ total $ listL [
   literalL '{' %> "{",
   exponentL %> "exp",
   signedDecimalL %> "decimal",
   decimalL %> "decimal",
   stringL %> "string",
   literalL '[' %> "[",
   literalL ']' %> "]",
   literalL ',' %> ",",
   tokenL "null" %> "null",
   tokenL "true" %> "true",
   tokenL "false" %> "false",
   whitespaceL,
   literalL '\"' %> "\"",
   literalL ':' %> ":",
   cardinalL,
   signedCardinalL %> "scard",
   literalL '}' %> "}"
   ]

exponentL :: Lexer
exponentL = (signedCardinalL <|> cardinalL)
         <&&> (literalL 'e' <|> literalL 'E')
         <&&> signedCardinalL

checkIntArray :: [Value] -> Bool
checkIntArray [(I _)] = True
checkIntArray [_] = False
checkIntArray ((I _) : is) = checkIntArray is
checkIntArray (_ : is) = False

checkBoolArray :: [Value] -> Bool
checkBoolArray [(B _)] = True
checkBoolArray [_] = False
checkBoolArray ((B _) : is) = checkBoolArray is
checkBoolArray (_ : is) = False

checkFloatArray :: [Value] -> Bool
checkFloatArray [(D _)] = True
checkFloatArray [_] = False
checkFloatArray ((D _) : is) = checkFloatArray is
checkFloatArray (_ : is) = False

checkStringArray :: [Value] -> Bool
checkStringArray [(D _)] = True
checkStringArray [_] = False
checkStringArray ((D _) : is) = checkStringArray is
checkStringArray (_ : is) = False

comp :: JObject -> Value -> Bool
-- empty schema
comp (Object ([])  ) _ = True
-- terminal value schemas
comp (Object [StringObject {propertyString = "type", propertyValue = (S "int")}]) (I _) = True
comp (Object [StringObject {propertyString = "type", propertyValue = (S "array")}, StringObject {propertyString = "elements", propertyValue = (O (Object [StringObject {propertyString = "type", propertyValue = S "int"}]) )}]) (A a) = checkIntArray a
comp (Object [StringObject {propertyString = "type", propertyValue = (S "bool")}]) (B _) = True
comp (Object [StringObject {propertyString = "type", propertyValue = (S "array")}, StringObject {propertyString = "elements", propertyValue = (O (Object [StringObject {propertyString = "type", propertyValue = S "bool"}]) )}]) (A a) = checkBoolArray a
comp (Object [StringObject {propertyString = "type", propertyValue = (S "float")}]) (D _) = True
comp (Object [StringObject {propertyString = "type", propertyValue = (S "array")}, StringObject {propertyString = "elements", propertyValue = (O (Object [StringObject {propertyString = "type", propertyValue = S "float"}]) )}]) (A a) = checkFloatArray a
comp (Object [StringObject {propertyString = "type", propertyValue = (S "string")}]) (S _) = True
comp (Object [StringObject {propertyString = "type", propertyValue = (S "array")}, StringObject {propertyString = "elements", propertyValue = (O (Object [StringObject {propertyString = "type", propertyValue = S "string"}]) )}]) (A a) = checkStringArray a
--comp (Object (StringObject {propertyString = "type", propertyValue = (S "object")} : os )) orig@(O (Object {getStringObjects = (prop:props)})) = True && comp os orig
comp (_) (D _) = False
comp (_) (B _) = False
comp (_) (I _) = False
comp (_) (S _) = False
comp (_) (A _) = False
comp _ _ = False

getCustomSchemas :: [Property] -> [Property]
getCustomSchemas [] = []
getCustomSchemas ( (StringObject {propertyString = "schemas", propertyValue = pv}) : ss ) = getStringObjects $ getObject pv
getCustomSchemas ( (_:ss) ) = getCustomSchemas ss

getCustomSchemas' :: [Property] -> [(String, [Property])]
getCustomSchemas' [( StringObject {propertyString = a, propertyValue = O o} )] = [(a, getStringObjects o)]
getCustomSchemas' (( StringObject {propertyString = a, propertyValue = O o} ) : ss) = (a, getStringObjects o) : getCustomSchemas' ss

-- Main for part 3
main3 :: IO ()
main3 = do

   jsonHandle <- readFile "data/person.json"
   let jsonFileText = jsonHandle
   let jsonFilePrelex = preLex jsonFileText
   
   let firstLex = valueL jsonFilePrelex

   putStrLn "File parse:"
   case firstLex of
      OK (lex,_) -> do
         case objectP lex of
            OK (parseTree, _) -> do
               print parseTree

            _ -> print $ objectP lex
      _ -> print firstLex

   putStrLn ""

-- Main for part 4
main :: IO ()
main = do
   
   jsonHandle <- readFile "data/int.json"
   schemaHandle <- readFile "schema/int-schema.json"
   let schema = schemaHandle
   let jsonFileText = jsonHandle
   let jsonFilePrelex = preLex jsonFileText
   
   let firstLex = valueL jsonFilePrelex

   putStrLn "File parse:"
   case firstLex of
      OK (lex,_) -> do
         case objectP lex of
            OK (parseTree, _) -> do
               print parseTree

               case valueL $ preLex schema of
                  OK (schemaLex,_) -> do
                     case objectP schemaLex of
                        OK (schemaParse, _) -> do
                           putStrLn "Schema parse:"
                           print schemaParse
                           --print (getStringObjects $ getObject schemaParse)
                           --putStrLn "Custom schemas:"
                           --print $ getCustomSchemas' $ getCustomSchemas $ getStringObjects $ getObject schemaParse
                           putStrLn "Valid against schema?"
                           print $ comp ( getObject schemaParse ) parseTree

                        _ -> print $ objectP schemaLex
                  
                  _ -> print $ valueL $ preLex schema

            _ -> print $ objectP lex
      _ -> print firstLex

   putStrLn ""
