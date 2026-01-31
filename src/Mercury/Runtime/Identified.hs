module Mercury.Runtime.Identified (
    Identified,
    UniqueID,
    genUniqueID,
    identify,
    insert,
    IdentifiedSet,
    UniqueIDStore,
    newStore,
    newStoreIO,
) where

import Control.Concurrent.STM
import Data.Hashable
import qualified StmContainers.Map as SM

newtype UniqueID = UniqueID {getUniqueID :: Int}
    deriving (Eq, Ord, Show, Num, Hashable)

type UniqueIDStore = TVar UniqueID

newStore :: STM UniqueIDStore
newStore = newTVar 0

newStoreIO :: IO UniqueIDStore
newStoreIO = newTVarIO 0

genUniqueID :: UniqueIDStore -> STM UniqueID
genUniqueID uidVar = do
    uid <- readTVar uidVar
    let newUid = uid + 1
    writeTVar uidVar newUid
    return uid

data Identified a = Identified UniqueID a

type IdentifiedSet a = SM.Map UniqueID a

identify :: UniqueIDStore -> a -> STM (Identified a)
identify uidStore val = (`Identified` val) <$> genUniqueID uidStore

insert :: Identified a -> IdentifiedSet a -> STM ()
insert (Identified uid val) = SM.insert val uid
