
-- Define all the sprites here. They just take a lot of time, hardcoding them all. 

module Sprites (
    loadSpriteMap
)
where

import Types
import Rendering

import Data.Array.Repa.IO.BMP
import Data.Array.Repa
import qualified Data.Map as M
import Data.List (nub)

loadSpriteMap :: IO (M.Map String Sprite)
loadSpriteMap = do
    let sheetPaths = nub $ fmap spcPath allSpriteConstructs
    sheets <- fmap (fmap addAlpha) $ mapM (\x -> readImageFromBMP x >>= \(Right a) -> return (x,a)) sheetPaths
    M.fromList $ 
        fmap
            (\sprite@(SpriteConstruct name path lst) -> constructSprite sprite sheets) 
            allSpriteConstructs

constructSprite :: SpriteConstruct -> [(FilePath,TileArr)] -> Sprite
constructSprite (SpriteConstruct name path frameAssocs) path2pic =
    let Just pic = lookup path path2pic
        frameMap = M.fromList $ fmap (\(frameID,(coords,size)) -> (frameID,takeSection coords size pic)) frameAssocs
    in
        Sprite frameMap

s = (16,16) --standard size
path = "assets/sprites/charsprites.bmp" --standard sprite sheet
-- would be nice to have standard frame locations generated from distance off the start
standard :: [Coord] -> [(FrameID,(Coord,Coord))] --standard frame locations generated from list (wait, no, only work on individual sheets)
standard (init:[]) = 
    (Init,((0,0),s))
    : []

m   (x,y) = (x*16,y*16)
n k (x,y) = (x*k,y*k)

--absol = 
--    SpriteConstruct
--        "Absol"
--        path
--        $
--        (Init,((0,0),s))
--        : []

--the ones I don't have a special place for
others = 
    SpriteConstruct
        "At"
        "assets/sprites/at.bmp"
        [(Init,((0,0),s)),
         (Nothing,((16,0),s)),
         (Nothing1,(n 16(1,0),s)),
         (Nothing2,(m(1,0),s))]
    : SpriteConstruct
        "Test1"
        "assets/sprites/charsprites.bmp"
        []
    : []

allSpriteConstructs =
    {-absol:-}others
