module ICFPC2018.Solvers.ColumnSolver where

<<<<<<< HEAD
import ICFPC2018.Types
import ICFPC2018.Tensor3 (BoundingBox)
import qualified ICFPC2018.Tensor3 as T3

import Linear.V3 (V3(..))
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Control.Monad.State.Strict
import Control.Arrow (first)
import Control.Monad.Loops

type NBot = Int
type LayerID = Int

data ColumnSolverModel = ColumnSolverModel {
        bots  :: ![Bot],
        model :: !Model,
        layer :: !Int
    } deriving (Show, Eq)

adjacentVoxels :: VolatileCoordinate -> [VolatileCoordinate]
adjacentVoxels c = (c +) <$> [
        V3 (-1)  0 (-1),
        V3 (-1)  0   0,
        V3 (-1)  0   1,
        V3   0   0 (-1),
        V3   0   0   1,
        V3   1   0 (-1),
        V3   1   0   0,
        V3   1   0   1
    ]

grounded :: Model -> VolatileCoordinate -> Bool
grounded model pos = model T3.! pos

generateBots :: NBot -> BoundingBox -> State ColumnSolverModel ()
generateBots nbot ((V3 x0 y0 z0), (V3 x1 y1 z1)) = do
    state@(ColumnSolverModel {..}) <- get
    put $ state {
        bots = bots ++ newBots
    }
    where
        nz = (z1 - z0) `div` 3 + (if (z1 - z0) `mod` 3 /= 0 then 1 else 0)
        nx = (x1 - x0) `div` 3 + (if (x1 - x0) `mod` 3 /= 0 then 1 else 0)
        xs = [x0 + 1, x0 + 4 .. x0 + 1 + 3 * nx]
        zs = [z0 + 1, z0 + 4 .. z0 + 1 + 3 * nz]
        newBots = map (\(idx,pos) -> Bot idx pos IS.empty) $ zip [1..nbot] botCrd
        botCrd = map (\(x,z) -> (V3 x y0 z)) $ concat $ map (\z' -> map (\x' -> (z', x')) xs) zs

fillVoxel :: Model -> BotPos -> VolatileCoordinate -> State ColumnSolverModel Command
fillVoxel task pos voxel = do
    if task T3.! voxel
        then
            do
                state@(ColumnSolverModel {..}) <- get
                put $ state { model = T3.update model [(voxel, True)] }
                return $ Fill $ voxel - pos
        else
            return $ Wait

fillLayer :: Model -> State ColumnSolverModel Trace
fillLayer task = do
    ColumnSolverModel {..} <- get
    let botCommands pos = fst <$> runState <$> mapM (fillVoxel task pos) $ adjacentVoxels pos
        indexedBotCommands idx pos = mapM (\cmd -> (idx, cmd)) $ botCommands pos
        allBotCommands = mapM (\(Bot idx pos _) -> indexedBotCommands idx pos)
        in mapM (M.fromList) $ mapM (L.transpose) $ allBotCommands bots

fillUnderBot :: Model -> State ColumnSolverModel Trace
fillUnderBot task = do
    ColumnSolverModel {..} <- get
    return $ M.fromList <$> (L.transpose $ map (\(Bot idx pos _) -> map (\cmd -> (idx, cmd)) $ mapM (fillVoxel task pos) $ [pos + (V3 0 (-1) 0)]) bots)

moveUp :: State ColumnSolverModel Trace
moveUp = do
    state@(ColumnSolverModel {..}) <- get
    put $ state { layer = layer + 1 }
    return $ M.fromList $ map (\(Bot idx (V3 x y z) _) -> (idx, SMove (V3 0 1 0)))

processLayer :: Model -> State ColumnSolverModel ()
processLayer task = do
    ColumnSolverModel {..} <- get
    when (layer /= 0) $ fillUnderBot task
    fillLayer task
    moveUp

defaultColumnSolver :: Model -> Bot -> ColumnSolverModel
defaultColumnSolver task bot = ColumnSolverModel { bots = [bot], model = T3.replicate (T3.size task) False, layer = 0 }

solve' :: ColumnSolverModel -> Model -> NBot -> BoundingBox -> State ColumnSolverModel Trace
solve' task nbot bbox@((V3 _ by0 _), (V3 _ by1 _)) = do
    generateBots nbot bbox
    state@(ColumnSolverModel {..}) <- get
    put $ state { layer = by0 }
    return $ map (++) $ whileM (layer <= by1) $ processLayer task

solve :: Model -> BotPos -> NBot -> Trace
solve task pos nbot =
    if needBots > nbot
        then runState $ solve' (defaultColumnSolver task $ primaryBot pos nbot) task nbot bbox
        else error "not implemented!"
    where
        bbox@((V3 bx0 by0 bz0), (V3 bx1 by1 bz1)) = T3.boundingBox task id
        bdx = abs $ bx1 - bx0
        bdz = abs $ bz1 - bz0
        needBots = bdx * bdz `div` 9
