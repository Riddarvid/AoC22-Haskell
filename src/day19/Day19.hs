{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Day19 (solve) where
import           Control.Monad.State (MonadState (get), State, evalState,
                                      modify)
import           Data.Map            (Map, (!), (!?))
import qualified Data.Map            as Map
import           Debug.Trace         (trace)
import           Solution            (Solution (I, S))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    blueprints = parseBlueprints input
    --part1 = solvePart1 blueprints
    part1 = 0
    part2 = solvePart2 blueprints

data Material = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord)

allMaterials :: [Material]
allMaterials = [Geode, Obsidian, Clay, Ore]

type Blueprint = Map Material [(Material, Integer)]

parseBlueprints :: String -> [Blueprint]
parseBlueprints input = map parseBlueprint $ lines input

parseBlueprint :: String -> Blueprint
parseBlueprint blueprintStr = Map.fromList [(Ore, oreCost), (Clay, clayCost), (Obsidian, obsidianCost), (Geode, geodeCost)]
  where
    tokens = words blueprintStr
    intAt n = read $ tokens !! n
    oreCost = [(Ore, intAt 6)]
    clayCost= [(Ore, intAt 12)]
    obsidianCost= [(Ore, intAt 18), (Clay, intAt 21)]
    geodeCost= [(Ore, intAt 27), (Obsidian, intAt 30)]

-- Part 1

solvePart1 :: [Blueprint] -> Integer
solvePart1 blueprints = sum $ zipWith qualityLevel [1..] blueprints

qualityLevel :: Integer -> Blueprint -> Integer
qualityLevel blueprintId blueprint = blueprintId * maxGeodes 24 blueprint

-- Part 2

solvePart2 :: [Blueprint] -> Integer
solvePart2 blueprints = product $ map (maxGeodes 32) $ take 3 blueprints

-- General

data WorldState = WorldState {
  timeLeft  :: Integer,
  materials :: Map Material Integer,
  robots    :: Map Material Integer
} deriving (Show, Eq, Ord)

data MemState = MemState{
  memMap        :: Map WorldState Integer,
  maxGuaranteed :: Integer
}

maxGeodes :: Integer -> Blueprint -> Integer
maxGeodes time blueprint =
  evalState (maxGeodes' blueprint WorldState {timeLeft = time, materials = startMaterials, robots = startRobots}) MemState {memMap = Map.empty, maxGuaranteed = 0}
  where
    startMaterials = Map.fromList $ map (, 0) allMaterials
    startRobots = Map.fromAscList [(Ore, 1), (Clay, 0), (Obsidian, 0), (Geode, 0)]

maxGeodes' :: Blueprint -> WorldState -> State MemState Integer
--maxGeodes' blueprint worldState | trace (show worldState) False = undefined
maxGeodes' _ worldState | timeLeft worldState == 0 = do
  let n = materials worldState ! Geode
  modify (\s -> s{memMap = Map.insert worldState n $ memMap s, maxGuaranteed = max n (maxGuaranteed s)})
  return $ materials worldState ! Geode
maxGeodes' blueprint worldState = do
  memState <- get
  case memMap memState !? worldState of
    Just n -> return n
    Nothing -> case let potential = maxPotential worldState in potential < maxGuaranteed memState && potential < 1 of
      True -> do
        modify (\s -> s{memMap = Map.insert worldState 0 $ memMap s})
        return 0
      False -> do
        modify (\s -> s{maxGuaranteed = max (materials worldState ! Geode) (maxGuaranteed s)})
        results <- mapM (maxGeodes' blueprint) nextStates
        return $ maximum results
  where
    affordable = filter (canAfford blueprint worldState) allMaterials
    possible = filter (not . meetsDemand blueprint worldState) affordable
    actions = if canDoNothing affordable (robots worldState)
      then map Just possible ++ [Nothing]
      else map Just possible
    nextStates = map (nextState blueprint worldState) actions

maxPotential :: WorldState -> Integer
maxPotential worldState = nGeo + nGeoBots * time + time * (time - 1) `div` 2
  where
    nGeoBots = robots worldState ! Geode
    nGeo = materials worldState ! Geode
    time = timeLeft worldState - timeUntil worldState Geode

timeUntil :: WorldState -> Material -> Integer
timeUntil worldState mat | robots worldState ! mat > 0 = 0
timeUntil worldState mat = 1 + timeUntil worldState (nextMat mat)

nextMat :: Material -> Material
nextMat Geode    = Obsidian
nextMat Obsidian = Clay
nextMat Clay     = Ore
nextMat Ore      = error "Hej"

canDoNothing :: [Material] -> Map Material Integer -> Bool
canDoNothing possibleBuilds currentRobots
  | clayN == 0 = length possibleBuilds < 2
  | obsN == 0 = length possibleBuilds < 3
  | otherwise = length possibleBuilds < 4
  where
    clayN = currentRobots ! Clay
    obsN = currentRobots ! Obsidian

meetsDemand :: Blueprint -> WorldState -> Material -> Bool
meetsDemand _ _ Geode = False
meetsDemand blueprint worldState mat = robots worldState ! mat >= maxCost
  where
    maxCost = Map.foldr
      (\costs acc -> case lookup mat costs of
        Nothing -> acc
        Just n  -> max acc n ) 0 blueprint

canAfford :: Blueprint -> WorldState -> Material -> Bool
canAfford blueprint state material = all (\(costMat, costVal) -> materials state ! costMat >= costVal) cost
  where
    cost = blueprint ! material

-- TODO also add resource generation and spawning a new robot
-- 1) Spend build resources
-- 2) Generate new resources
-- 3) Add new robot
-- 4) Spend one time
nextState :: Blueprint -> WorldState -> Maybe Material -> WorldState
nextState blueprint state material = case material of
  Nothing -> state{materials = materials',timeLeft = timeLeft state - 1}
    where
      materials' = generateResources (materials state) (robots state)
  Just material' -> state{materials = materials'', robots = Map.adjust (+ 1) material' (robots state), timeLeft = timeLeft state - 1}
    where
      cost = blueprint ! material'
      materials' = spendCost (materials state) cost
      materials'' = generateResources materials' (robots state)

generateResources :: Map Material Integer -> Map Material Integer -> Map Material Integer
generateResources = Map.foldrWithKey (\mat count acc -> Map.adjust (+ count) mat acc)

spendCost :: Map Material Integer -> [(Material, Integer)] -> Map Material Integer
spendCost = foldr (\(costMat, costVal) acc -> Map.adjust (\n -> n - costVal) costMat acc)
