module Day19Alt (solve) where
import           Data.List   (find, findIndex, maximumBy, minimumBy, sortBy)
import           Data.Map    (Map, (!), (!?))
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace (trace)
import           Graphs      (BFSOptions (BFSOptions, keepVisited, pruneFun),
                              reachableBFS)
import           Solution    (Solution (I))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    blueprints = parseBlueprints input
    part1 = solvePart1 blueprints
    part2 = solvePart2 blueprints

data Material = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord)

allMaterials :: [Material]
allMaterials = [Geode, Obsidian, Clay, Ore]

type Blueprint = Map Material [(Material, Integer)]

-- Parsing

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

-- Part1

data WorldState = WorldState {
  timeLeft  :: Integer,
  materials :: Map Material Integer,
  robots    :: Map Material Integer
} deriving (Show, Eq, Ord)

solvePart1 :: [Blueprint] -> Integer
solvePart1 blueprints = sum $ zipWith qualityLevel [1..] blueprints

qualityLevel :: Integer -> Blueprint -> Integer
qualityLevel blueprintId blueprint = blueprintId * maxGeodes 24 blueprint

solvePart2 :: [Blueprint] -> Integer
solvePart2 blueprints = product $ map (maxGeodes 32) $ take 3 blueprints

maxGeodes :: Integer -> Blueprint -> Integer
maxGeodes time blueprint = Set.findMax $ Set.map (\s -> materials s ! Geode) $ bestStates time blueprint


bestStates :: Integer -> Blueprint -> Set WorldState
bestStates time blueprint = reachableBFS startState (adjacency blueprint) options
  where
    startState = WorldState {timeLeft = time, materials = startMaterials, robots = startRobots}
    startMaterials = Map.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    startRobots = Map.fromList [(Ore, 1), (Clay, 0), (Obsidian, 0), (Geode, 0)]
    options = BFSOptions{pruneFun = Just $ prune blueprint, keepVisited = False}

-- Adjacency

adjacency :: Blueprint -> WorldState -> [WorldState]
adjacency _ worldState | timeLeft worldState == 0 = []
adjacency blueprint worldState = nextStates
  where
    affordable = filter (canAfford blueprint worldState) allMaterials
    possible = filter (not . meetsDemand blueprint worldState) affordable
    actions = if canDoNothing affordable (robots worldState)
      then map Just possible ++ [Nothing]
      else map Just possible
    nextStates = map (nextState blueprint worldState) actions

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


-- Pruning: Keeps the best 1000 candidates from each layer

prune :: Blueprint -> Set WorldState -> Set WorldState
prune blueprint states = states'
  where
    maxG = maxGuaranteed blueprint states
    states' = Set.filter (\state -> maxPotential blueprint state >= maxG) states
    --states'' = Set.fromList $ take 100000 $ sortBy (flip (\a b -> compare (guaranteed a) (guaranteed b))) (Set.toList states')

maxGuaranteed :: Blueprint -> Set WorldState -> Integer
maxGuaranteed blueprint = Set.fold (max . guaranteed blueprint) 1

guaranteed :: Blueprint -> WorldState -> Integer
guaranteed blueprint state = materials (fromJust $ find (\s -> timeLeft s == 0) $ iterate (buildExpensive blueprint) state) ! Geode

buildExpensive :: Blueprint -> WorldState -> WorldState
buildExpensive blueprint state
  | canAfford blueprint state toBuild = nextState blueprint state (Just toBuild)
  | otherwise = nextState blueprint state Nothing
  where
    toBuild = fromJust $ find (\mat -> robots state ! nextMat mat > 0) allMaterials

maxPotential :: Blueprint -> WorldState -> Integer
maxPotential blueprint worldState = nGeo + nGeoBots * time + time * (time - 1) `div` 2
  where
    nGeoBots = robots worldState ! Geode
    nGeo = materials worldState ! Geode
    time = timeLeft worldState - timeUntil blueprint worldState Geode

-- An upper bound on the time needed
timeUntil :: Blueprint -> WorldState -> Material -> Integer
timeUntil _ worldState mat | robots worldState ! mat > 0 = 0
timeUntil blueprint worldState mat = time + timeUntil blueprint worldState (nextMat mat)
  where
    nBots = robots worldState ! nextMat mat
    nNextMat = materials worldState ! nextMat mat
    matCost = fromJust $ lookup (nextMat mat) (blueprint ! mat)
    time = toInteger $ fromJust $ findIndex (\(n, _) -> n >= matCost) $ iterate (\(n, nB) -> (n + nB, nB + 1)) (nNextMat, nBots)

nextMat :: Material -> Material
nextMat Geode    = Obsidian
nextMat Obsidian = Clay
nextMat Clay     = Ore
nextMat Ore      = error "Hej"
{-cmpScore :: WorldState -> WorldState -> Ordering
cmpScore a b = compare (score a)  (score b)

score :: WorldState -> Integer
score state = nOre + 10 * nClay + 100 * nObs + 1000 * nGeo
  where
    robots' = robots state
    nOre = robots' ! Ore
    nClay = robots' ! Clay
    nObs = robots' ! Obsidian
    nGeo = robots' ! Geode-}

-- Adjacency

