{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
{- CRUD operations for PersonRoute. See Json.hs for full code. -}
module PersonCrud where

import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Typeable(Typeable)
import Data.Data (Data)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), (.:), (.=), object)
import Data.Aeson.TH (deriveJSON, defaultOptions)

-- A simple database of `Person`
data Person = Person
  { name :: Text
  , age  :: Int
  }
$(deriveJSON defaultOptions ''Person)

type People = IntMap Person

-- The Master Route
data PeopleRoute = PeopleRoute
  { peopleRef :: IORef People }

-- Create an initial master route instance
-- Initially our DB is empty
initRoute :: IO PeopleRoute
initRoute = do
  ref <- newIORef $ IM.singleton 0 $ Person "Anon" 20
  return $ PeopleRoute ref

-- Create a new person
newPerson :: Person -> PeopleRoute -> IO Int
newPerson v (PeopleRoute ref) = do
  peeps <- readIORef ref
  let k = nextKey peeps
  writeIORef ref $ IM.insert k v peeps
  return k
  where
    nextKey peeps
      | IM.null peeps = 0
      | otherwise = fst (IM.findMax peeps) + 1

-- Get data for a specifc person
getPerson :: Int -> PeopleRoute -> IO (Maybe Person)
getPerson i (PeopleRoute ref) = do
  peeps <- readIORef ref
  return $ IM.lookup i peeps

-- Update a person
-- Silently ignores if person isn't already in the DB
updatePerson :: Int -> Person -> PeopleRoute -> IO ()
updatePerson i p (PeopleRoute ref) = do
  peeps <- readIORef ref
  writeIORef ref $ IM.update (const $ Just p) i peeps

-- Delete a person
-- Silently ignores if person isn't already in the DB
deletePerson :: Int -> PeopleRoute -> IO ()
deletePerson i (PeopleRoute ref) = do
  peeps <- readIORef ref
  writeIORef ref $ IM.delete i peeps
