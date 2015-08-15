{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
{- CRUD operations for PersonRoute. See Main.hs for full code. -}
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

-- The Master Route
data PeopleDB = PeopleDB
  { peopleRef :: IORef (IntMap Person) }

-- Create an initial master route instance
-- Initially our DB is empty
initPeopleDB :: IO PeopleDB
initPeopleDB = do
  ref <- newIORef $ IM.singleton 0 $ Person "Anon" 20
  return $ PeopleDB ref

-- Create a new person
newPerson :: Person -> PeopleDB -> IO Int
newPerson v (PeopleDB ref) = do
  peeps <- readIORef ref
  let k = nextKey peeps
  writeIORef ref $ IM.insert k v peeps
  return k
  where
    nextKey peeps
      | IM.null peeps = 0
      | otherwise = fst (IM.findMax peeps) + 1

-- Get data for a specifc person
getPerson :: Int -> PeopleDB -> IO (Maybe Person)
getPerson i (PeopleDB ref) = do
  peeps <- readIORef ref
  return $ IM.lookup i peeps

-- Update a person
-- Silently ignores if person isn't already in the DB
updatePerson :: Int -> Person -> PeopleDB -> IO ()
updatePerson i p (PeopleDB ref) = do
  peeps <- readIORef ref
  writeIORef ref $ IM.update (const $ Just p) i peeps

-- Delete a person
-- Silently ignores if person isn't already in the DB
deletePerson :: Int -> PeopleDB -> IO ()
deletePerson i (PeopleDB ref) = do
  peeps <- readIORef ref
  writeIORef ref $ IM.delete i peeps
