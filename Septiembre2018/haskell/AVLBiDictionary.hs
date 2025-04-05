-------------------------------------------------------------------------------
-- Apellidos, Nombre: 
-- Titulacion, Grupo: 
--
-- Estructuras de Datos. Grados en Informatica. UMA.
-------------------------------------------------------------------------------

module AVLBiDictionary( BiDictionary
                      , empty
                      , isEmpty
                      , size
                      , insert
                      , valueOf
                      , keyOf
                      , deleteByKey
                      , deleteByValue
                      , toBiDictionary
                      , compose
                      , isPermutation
                      , orbitOf
                      , cyclesOf
                      ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Set.BSTSet               as S

import           Data.List                               (intercalate, nub,
                                                          (\\))
import           Data.Maybe                              (fromJust, fromMaybe,
                                                          isJust)
import           Test.QuickCheck


data BiDictionary a b = Bi (D.Dictionary a b) (D.Dictionary b a)

-- | Exercise a. empty, isEmpty, size

empty :: (Ord a, Ord b) => BiDictionary a b
empty = Bi (D.empty) (D.empty)

isEmpty :: (Ord a, Ord b) => BiDictionary a b -> Bool
isEmpty (Bi dk dv) = if D.isEmpty dk then True else False

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi dk dv) = if D.isEmpty dk then 0 else (D.size dk)

-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert k v (Bi dk dv)
  | D.isDefinedAt k dk && D.isDefinedAt v dv = Bi (D.insert k v (D.delete k dk)) (D.insert v k (D.delete v dv))
  | D.isDefinedAt k dk = Bi (D.insert k v (D.delete k dk)) (D.insert v k dv)
  | D.isDefinedAt v dv = Bi (D.insert k v dk) (D.insert v k (D.delete v dv))
  | otherwise = Bi (D.insert k v dk) (D.insert v k dv)

-- | Exercise c. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k bidi@(Bi dk dv)
  | D.isDefinedAt k dk = D.valueOf k dk
  | otherwise = Nothing

-- | Exercise d. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v bidi@(Bi dk dv)
  | D.isDefinedAt v dv = D.valueOf v dv
  | otherwise = Nothing

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k (Bi d1 d2)
  | not (D.isDefinedAt k d1) = (Bi d1 d2)
  | otherwise = Bi (D.delete k d1) (D.delete (fromJust (D.valueOf k d1)) d2)

-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v bidi@(Bi dk dv)
  | not (D.isDefinedAt v dv) = bidi
  | otherwise = Bi (D.delete (fromJust(D.valueOf v dv)) dk) (D.delete v dv)

-- | Exercise g. toBiDictionary

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict 
  | not (isInyective dict) = error "el diccionario no es inyectivo"
  | D.isEmpty dict = Bi (D.empty) (D.empty)
  | isInyective dict = Bi dict (aux (D.keysValues dict) D.empty)
    where
      aux [] dictaux = dictaux
      aux ((k,v):xs) dictaux = aux xs (D.insert v k dictaux)

isInyective ::(Eq b) => D.Dictionary a b -> Bool
isInyective dict = if (length (nub (D.values dict))) == (length (D.keys dict)) then True else False 
-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose b1@(Bi dk1 dv1) b2@(Bi dk2 dv2) = toBiDictionary (aux (D.keysValues dk1) dk2 (D.empty))
  where
    aux [] dict2 dictaux = dictaux
    aux ((k,v):xs) dict2 dictaux
      | D.isDefinedAt v dict2 = aux xs dict2 (D.insert k (fromJust(D.valueOf v dict2)) dictaux)
      | otherwise = aux xs dict2 dictaux 

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi dk dv)
  | keysSet == valuesSet = True
  | otherwise            = False
  where
    keysSet   = S.fromList (D.keys dk)
    valuesSet = S.fromList (D.keys dv)

    



-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf = undefined

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf = undefined

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
