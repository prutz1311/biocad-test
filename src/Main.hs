{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text                        (Text)
import           Database.Bolt
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                       (listToMaybe)
import           Data.Default                     (def)
import           Control.Monad.Trans.Maybe        (MaybeT(..))
import           Control.Monad.Trans.Except       (runExceptT)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad                    (when)
import           Control.Monad.IO.Class           (liftIO)



data Molecule = Molecule {
  m_id :: Int,
  m_smiles :: Text,
  m_iupacName :: Text
  } deriving (Eq, Show)

data Reaction = Reaction {
  r_id :: Int,
  r_name :: Text
  } deriving (Eq, Show)

data Catalyst = Catalyst {
  c_id :: Int,
  c_smiles :: Text,
  c_name :: Maybe Text
  } deriving (Eq, Show)

data PRODUCT_FROM = PRODUCT_FROM {
  amount :: Double
  } deriving (Eq, Show)

data ACCELERATE = ACCELERATE {
  temperature :: Double,
  pressure :: Double
 } deriving (Eq, Show)


addMolecule :: Molecule -> BoltActionT IO ()
addMolecule m = queryP_ query params
  where query = "CREATE (:Molecule { id: {id}, smiles: {smiles}, iupacName: {iupacName} })"
        params = Map.fromList [("id", I (m_id m)),
                               ("smiles", T (m_smiles m)),
                               ("iupacName", T (m_iupacName m))]

addCatalyst :: Catalyst -> BoltActionT IO ()
addCatalyst cat = queryP_ query params
  where query = "CREATE (:Catalyst { id: {id}, smiles: {smiles}, name: {name} })"
        params_ = Map.fromList [("id", I (c_id cat)),
                                ("smiles", T (c_smiles cat))]
        params = maybe params_ (\name -> Map.insert "name" (T name) params_) (c_name cat)

addReaction :: Int             -- ^ Left-hand side molecule id
            -> Int             -- ^ Left-hand side molecule id
            -> Int             -- ^ Right-hand side molecule id
            -> Int             -- ^ Catalyst id
            -> PRODUCT_FROM    -- ^ PRODUCT_FROM object to build relationship from
            -> ACCELERATE      -- ^ ACCELERATE object to build relationship from
            -> Reaction        -- ^ Reaction to insert
            -> BoltActionT IO (Either String ())
addReaction leftMol1 leftMol2 rightMol catalyst prod acc reaction = runExceptT $ do
  records <- lift $ queryP queryCheck params
  let record = Prelude.head records
  I count <- at record "count"
  when (count == 0) $ fail "Specified molecules or relationships not found"
  lift $ queryP_ queryInsert params
  where queryCheck =
          "MATCH (lm1: Molecule {id: {lm1id}}), (lm2: Molecule {id: {lm2id}}), (rm: Molecule {id: {rmid}}), (cat: Catalyst {id: {c_id}}) " <>
          "RETURN count(*) AS count"

        queryInsert =
          "MATCH (lm1: Molecule {id: {lm1id}}), (lm2: Molecule {id: {lm2id}}), (rm: Molecule {id: {rmid}}), (cat: Catalyst {id: {c_id}}) " <>
          "CREATE (r: Reaction {id: {r_id}, name: {r_name}}) " <>
          "CREATE (lm1)-[:REAGENT_IN]->(r) " <>
          "CREATE (lm2)-[:REAGENT_IN]->(r) " <>
          "CREATE (r)-[:PRODUCT_FROM {amount: {amount}}]->(rm) " <>
          "CREATE (cat)-[:ACCELERATE {temperature: {temp}, pressure: {pres}}]->(r) "

        params = Map.fromList [("lm1id", I leftMol1),
                               ("lm2id", I leftMol2),
                               ("rmid", I rightMol),
                               ("c_id", I catalyst),
                               ("r_id", I (r_id reaction)),
                               ("r_name", T (r_name reaction)),
                               ("amount", F amount),
                               ("temp", F temp),
                               ("pres", F pres)]

        PRODUCT_FROM amount = prod
        ACCELERATE temp pres = acc
  


toReaction :: Monad m => Value -> m Reaction
toReaction v = do
  Node identity _ props <- exact v
  reactionName <- at props "name" >>= exact
  return Reaction { r_id = identity, r_name = reactionName }

findReaction :: Int -> BoltActionT IO (Maybe Reaction)
findReaction id = do
  records <- queryP query params
  let record = listToMaybe records
  node <- traverse (`at` "reaction") record
  traverse toReaction node
  where query = "MATCH (reaction: Reaction {id: {id}}) RETURN reaction"
        params = Map.singleton "id" (I id)

findPath :: Int -> Int -> BoltActionT IO (Maybe [Reaction])
findPath moleculeIdStart moleculeIdEnd = do
  records <- queryP query params
  let record = listToMaybe records
  runMaybeT $ do
    L nodes <- MaybeT $ traverse (`at` "reactions") record
    traverse toReaction nodes
  where query :: Text
        query =
          "MATCH (a: Molecule {id: {id1}})-[:REAGENT_IN|PRODUCT_FROM*]->(b: Molecule {id: {id2}}) " <>
          "MATCH (reaction:Reaction) " <> 
          "RETURN collect(reaction) AS reactions " <>
          "ORDER BY size(reactions) ASC "
        params = Map.fromList [("id1", I moleculeIdStart),
                               ("id2", I moleculeIdEnd)]


boltConfig :: BoltCfg
boltConfig = def

runPathQuery :: Int -> Int -> IO ()
runPathQuery id1 id2 = do
  pipe <- connect boltConfig
  r <- run pipe (findPath id1 id2)
  print r
  
main :: IO ()
main = do
  pipe <- connect boltConfig
  run pipe $ do
    liftIO $ putStrLn "Adding data"
    mapM_ addMolecule molecules
    mapM_ addCatalyst catalysts
    addReaction 1 2 3 101 prod acc reaction
    addReaction 3 4 5 102 prod acc reaction { r_id = 50 }
    addReaction 5 7 8 103 prod acc reaction { r_id = 51 }
    addReaction 8 9 10 104 prod acc reaction { r_id = 52 }
    liftIO $ putStrLn "Finding reaction path between molecules 2 and 10"
    r <- findPath 2 10
    liftIO $ print r
  where molecules = [Molecule 1 "a" "a",
                     Molecule 2 "b" "b",
                     Molecule 3 "c" "c",
                     Molecule 4 "4" "4",
                     Molecule 5 "5" "5",
                     Molecule 6 "6" "6",
                     Molecule 7 "7" "7",
                     Molecule 8 "8" "8",
                     Molecule 9 "9" "9",
                     Molecule 10 "10" "10",
                     Molecule 11 "11" "11",
                     Molecule 12 "12" "12",
                     Molecule 13 "13" "13",
                     Molecule 13 "13" "13",
                     Molecule 14 "14" "14",
                     Molecule 15 "15" "15",
                     Molecule 16 "16" "16",
                     Molecule 17 "17" "17",
                     Molecule 18 "18" "18",
                     Molecule 19 "19" "19",
                     Molecule 20 "20" "20",
                     Molecule 21 "21" "21"]
        catalysts = [Catalyst 101 "d" (Just "d"),
                     Catalyst 102 "102" (Just "102"),
                     Catalyst 103 "103" (Just "103"),
                     Catalyst 104 "104" (Just "104"),
                     Catalyst 105 "105" (Just "105")]
        prod = PRODUCT_FROM 0.1
        acc = ACCELERATE 0 0
        reaction = Reaction 1 "r1"
