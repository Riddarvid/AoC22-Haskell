module Day16 (solve) where
import           Control.Monad.State (State, evalState, get)
import           Data.List           (delete, find, sort)
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           GHC.Data.Maybe      (isNothing)
import           Graphs              (distancesBFS)
import           Solution            (Solution (I, S))
import           StringUtils         (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    valves = parseInput input
    distanceMap = generateDistances valves
    valveMap = Map.fromList $ map (\valve -> (vName valve, valve)) $ filter (\valve -> vFlow valve /= 0 || vName valve == "AA") valves
    part1 = maxFlow 1 30 distanceMap valveMap
    part2 = maxFlow 1 30 distanceMap valveMap

data Valve = Valve{
  vName      :: String,
  vFlow      :: Integer,
  vNeighbors :: [String]
} deriving (Show, Eq, Ord)


type DistanceMap = Map String (Map String Integer)

-- Parsing

parseInput :: String -> [Valve]
parseInput input = map parseValve $ lines input

parseValve :: String -> Valve
parseValve str = Valve roomId rate roomIds
  where
    tokens = words str
    roomId = tokens !! 1
    rate = head $ getIntegers str
    roomIds = map (take 2) $ drop 9 tokens

-- Setup / graph building

getAdjacencyFun :: [Valve] -> Valve -> [Valve]
getAdjacencyFun valves valve = [valveMap ! neighbor | neighbor <- vNeighbors valve]
  where
    valveMap = Map.fromList [(vName valve', valve') | valve' <- valves]

generateDistances :: [Valve] -> DistanceMap
generateDistances valves = Map.fromList [(vName valve, distancesTo valve adjacencyFun) | valve <- startValve : nonZeroValves]
  where
    startValve = fromJust $ find (\valve -> vName valve == "AA") valves
    nonZeroValves = filter (\valve -> vFlow valve /= 0) valves
    adjacencyFun = getAdjacencyFun valves

distancesTo :: Valve -> (Valve -> [Valve]) -> Map String Integer
distancesTo startValve adjacencyFun = Map.mapKeys vName $ Map.filterWithKey (\valve _ -> vFlow valve /= 0) (distancesBFS startValve adjacencyFun)


-- General

-- Process: At each iteration, an actor chooses whether to move to a new free location or to stop working
-- If you stop working, you never start working again
-- Each actor keeps track of how much flow they have contributed
-- We're done when all actors have stopped working
-- If an actor is out of time it must stop working
-- An actor can only move to a location that has not yet been visited

data Actor = Actor (Maybe String) Integer
  deriving Show

data ActorState = ActorState [Actor] (Set String)
  deriving Show

type GraphState = (DistanceMap, Map String Valve)

maxFlow :: Int -> Integer -> DistanceMap -> Map String Valve -> Integer
maxFlow nActors time distanceMap valveMap = evalState (maxFlow' (ActorState (replicate nActors (Actor (Just "AA") time)) (Set.delete "AA" $ Map.keysSet valveMap))) (distanceMap, valveMap)

maxFlow' :: ActorState -> State GraphState Integer
--maxFlow' actorState | trace (show actorState) False = undefined
maxFlow' actorState@(ActorState actors _)
  | all (\(Actor room _) -> isNothing room) actors = return 0 -- If all actors have stopped working
  | otherwise = do
    nextStates <- getNextPossibleStates actorState  -- All combinations of new locations and stopping for all workers
    subFlows <- mapM maxFlow' nextStates            -- Recursive call to find the maximum flow from these sub-states
    currentFlow <- flowGenerated actorState
    return $ currentFlow + maximum subFlows

getNextPossibleStates :: ActorState -> State GraphState [ActorState]
getNextPossibleStates state@(ActorState [] _) = return [state]
getNextPossibleStates (ActorState (actor:actors) remaining) = do
  nextActors <- getPossibleRoutes actor remaining -- Should take available and return maybe
  states <- mapM (\actor'@(Actor room _) -> map (\(ActorState actors' remaining'') -> ActorState (actor' : actors') remaining'') <$> getNextPossibleStates (ActorState actors (remaining' room))) nextActors
  return $ concat states
  where
    remaining' room = case room of
      Nothing    -> remaining
      Just room' -> Set.delete room' remaining

getPossibleRoutes :: Actor -> Set String -> State GraphState [Actor]
getPossibleRoutes (Actor _ timeLeft) _ | timeLeft <= 0 = return [Actor Nothing 0]
getPossibleRoutes (Actor Nothing _) _ = return [Actor Nothing 0]
getPossibleRoutes actor remaining = do
  let available = Nothing : map Just (Set.toList remaining)
  mapM (getRoute actor) available

getRoute :: Actor -> Maybe String -> State GraphState Actor
getRoute (Actor _ _) Nothing = return $ Actor Nothing 0
getRoute (Actor Nothing _) _ = return $ Actor Nothing 0
getRoute (Actor room timeLeft) next = do
  (distanceMap, _) <- get
  let distance = (distanceMap ! fromJust room) ! fromJust next
  return $ Actor next (timeLeft - distance - 1)


flowGenerated :: ActorState -> State GraphState Integer
flowGenerated (ActorState actors _) = do
  flows <- mapM actorFlow $ filter (\(Actor _ timeLeft) -> timeLeft > 0) actors
  return $ sum flows
  where
    actorFlow :: Actor -> State GraphState Integer
    actorFlow (Actor room timeLeft) = case room of
      Nothing -> return 0
      Just room' -> do
        (_, valveMap) <- get
        return $ timeLeft * vFlow (valveMap ! room')


-- Old

maxFlow'' :: String -> Integer -> DistanceMap -> Map String Valve -> Integer
maxFlow'' currentRoom timeLeft distanceMap valveMap
  | null reachableRoutes = 0
  | otherwise = maximum [flowGenerated room travelCost + subFlow room travelCost | (room, travelCost) <- reachableRoutes]
  where
    reachableRoutes = [(room, travelCost) | room <- Map.keys valveMap, let travelCost = (distanceMap ! currentRoom) ! room, travelCost + 1 < timeLeft]
    subFlow room travelCost = maxFlow'' room (timeLeft - travelCost - 1) distanceMap (Map.delete room valveMap)
    flowGenerated room travelCost = (timeLeft - travelCost - 1) * vFlow (valveMap ! room)
