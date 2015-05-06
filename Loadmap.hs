

module Loadmap (
    loadEasyMap,
    loadFontmap,
    loadSprites
    --loadGameMap,
    --makeTileMap,
    --loadTilesetImage
) where



import Model
--import Rendering

import Data.Tiled
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP
import Codec.Picture.Repa
import Graphics.Gloss
import Data.Word
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import Data.Monoid
import Control.Monad
import Data.Either

{-
I should make a Map between tilesheet name and tilesheet image. Some of the maps will load redundant tilesheets.
-}

{-

-- and thus a tiledMap is loaded! and much is done with the map.

-- each of these FilePaths represent either a tileset or some other image
loadGameMap :: FilePath -> IO DungeonMap
loadGameMap mapPath = do
    map <- loadMapFile mapPath
    --let tsPaths = fmap (iSource . head . tsImages) $ mapTilesets map
    let tss = mapTilesets map
    tssWithArrs <- mapM (\ts -> loadTilesetImage ts >>= \x -> return (ts,x)) tss
    let tileMap = makeTileMap tssWithArrs
    let layers = mapLayers map
    let baseRefer floor coord = fmap tileGid $ M.lookup coord $ layerData (layers !! floor)
    let player = Player "player" 1 At (4,7) [] (Sprite $ M.singleton BlankFrame emptyTile)
    let size = (80,50)
    let dmap = DungeonMap {
        dmapTiledMap = map,
        dmapRefer = baseRefer,
        dmapFloors = (length layers),
        dmapInitEntities = Sq.empty,
        dmapTileRefer = (flip M.lookup $ tileMap),
        dmapDimensions = size
    }
    return dmap --(GameMap (Left dmap))

-}

loadEasyMap :: IO EasyMap
loadEasyMap = do
    let tilesetImagePath = "assets/image.bmp"
    let tiledMapPath = "assets/map.tmx"
    let emptyPixel = (1,1,1) :: (Word8,Word8,Word8)
    -- put that image in there or else
    --(Right img) <- readImageFromBMP tilesetImagePath
    -- img :: Array U DIM2 (Word8,Word8,Word8)
    -- img4 :: Tilearr
    img4 <- readImageFromBMP tilesetImagePath >>= return . computeUnboxedS . alphatize . flipHV . transpose . either (error . show) id
    -- put that map in there or else
    map <- loadMapFile tiledMapPath
    -- I will only use the first layer
    let morph = fmap tileGid . layerData . head $ mapLayers map
    let ats = initTileset img4
    return $ EasyMap 
        { emapGrid = morph
        , emapTileset = ats
        , emapTileGrid = fmap (tsMorph ats) morph
        }

initTileset :: Tilearr -> ATileset
initTileset image = 
    let arr = delay image
        (Z:.w:.h) = extent arr
        -- number of tiles wide and high. the if is so even one pixel will make a new tile
        wTiles = let (primary,rem) = divMod w 24 in primary + if rem == 0 then 0 else 1
        hTiles = let (primary,rem) = divMod h 24 in primary + if rem == 0 then 0 else 1
        f :: Word32 -> (Int,Int) 
        -- result is how many tiles down, how many tiles to the right
        f word = let (h',w') = divMod (fromIntegral word - 1) wTiles in (h',w')
        g :: (Int,Int) -> Tilearr
        -- result is delicious image data
        g (h,w) = computeUnboxedS $ extract (Z:.w*24:.h*24) (Z:.24:.24) arr
    in  ATileset (g . f) -- maybe I should compute this once and make a map...but this is easier

loadFontmap :: IO Pixelarr
loadFontmap = do
    img4 <- readImageFromBMP "assets/fontmap.bmp" >>= return . delay . computeUnboxedS . alphatize . flipHV . transpose . either (error . show) id
    --let img4 = computeUnboxedS {-$ addLines-} $ alphatize img
    return img4


-- flipv might be more accurate
flipHV arr = 
    let (Z:.x0:.y0) = extent arr
    in  backpermute (Z:.x0:.y0) (\(Z:.x:.y) -> (Z:.x:.y0-y)) arr

addLines arr =
    Repa.traverse arr id (\f (Z:.x:.y) -> if mod x 9 == 0 || mod y 16 == 0 then (200,100,100,255) else f (Z:.x:.y))

--alphatize :: Array r DIM2 (Word8,Word8,Word8) -> Pixelarr
alphatize img = Repa.map (\(r,g,b) -> if (r,g,b) == emptyPixel then (r,g,b,minBound) else (r,g,b,maxBound)) img

-- This is the transparency pixel, where any pixel of this color will lose its color and have an alpha of 0.
emptyPixel = (1,1,1)



-- Sprites

--emptyTile = computeUnboxedS $ fromFunction (Z:.0:.0) (const (0,0,0,0))
extract' a b c = computeUnboxedS $ extract a b c
s24pix = Z:.24:.24
get24 sh arr = extract' sh s24pix arr
-- I must load the sprites somehow and store them in hte world

-- I will have a single sprite sheet. Simple sprites there.

--a bunch of tilearrs that are referred to by strings, as well as a normal one

loadSprites :: IO (M.Map String Sprite)
loadSprites = do
    spriteArr <- readImageFromBMP "assets/spritemap.bmp" >>= return . alphatize . flipHV . transpose . either (error . show) id
    let arr = spriteArr
    let playerSprite = Sprite (get24 (Z:.0:.0) arr) $ M.fromList [("f1",get24 (Z:.0:.0) arr),("f2",get24 (Z:.24:.0) arr),("f3",get24 (Z:.48:.0) arr)]
    let emptySprite = Sprite emptyTile M.empty
    let dotSprite = Sprite (extract' (Z:.0:.48) s24pix spriteArr) mempty
    return $ M.fromList [("player",playerSprite),("empty",emptySprite),("dot",dotSprite)]




{- Doesn't load images multiple times
loadGameMap :: [(FilePath,Tilearr)] -> FilePath -> IO GameMap
loadGameMap loadedImages mapPath = do
    map <- loadMapFile mapPath
    --let tsPaths = fmap (iSource . head . tsImages) $ mapTilesets map
    let tss = mapTilesets map
    let f ts = case (lookup (iSource $ head $ tsImages ts) loadedImages) of 
            Just arr -> return (ts,arr)
            Nothing -> loadTilesetImage ts >>= \x -> return (ts,x)
    tssWithArrs <- mapM f tss
        -- for each path, if it exists retrieve the arr. 
    g . (loadedImages)
    let tileMap = makeTileMap tssWithArrs
    let layers = mapLayers map
    let baseRefer floor coord = fmap tileGid $ M.lookup coord $ layerData (layers !! floor)
    let dmap = DungeonMap baseRefer [] (flip M.lookup $ tileMap)
    return (GameMap (Left dmap))
-}

-- Not useful
--mergeTileMaps :: M.Map Word32 sTilearr -> M.Map Word32 Tilearr -> M.Map Word32 Tilearr
--mergeTileMaps tiles1 tiles2 = M.unionWith (id . const) tiles1 tiles2

--makes the map between tile ID and picture
--thankfully the only place I need to deal with this.
--good target for refactoring
{-
makeTileMap :: [(Tileset,Tilearr)] -> M.Map Word32 Tilearr -- this could also return a list or map of tile properties. maybe a map between gid and a map between property and value, though that sounds clunkys
makeTileMap tssWithArrs =    
    let (tss,tilesetArrays) = unzip tssWithArrs
        countTiles arr = let (Z:.x:.y) = extent arr in (fromIntegral $ (x `div` 16) * (y `div` 16)) :: Word32
        totalTiles = sum (fmap countTiles tilesetArrays)
        tilearrs = fmap 
                   (\gid -> let (which,loc) = gid2loc tss gid in ripTile (tilesetArrays !! which) loc)
                   [1..totalTiles] --1 because the IDs start with 1
        in
        M.fromList $ zip [1..totalTiles] tilearrs

addAlpha :: Array U DIM2 (Word8,Word8,Word8) -> Tilearr
addAlpha arr = let transparencyPixel = (0xAA,0xAA,0xAA) in
    computeUnboxedP' $ R.map (\(r,g,b) -> (r,g,b, if (r,g,b) == transparencyPixel then 0 else 255)) arr

loadTilesetImage :: Tileset -> IO Tilearr
loadTilesetImage ts =
    let filename = iSource . head $ tsImages ts
        imgIOEither = readImageRGBA $ "assets/" ++ filename
        imgIO = fmap (either error id) imgIOEither  --just error if it fails
        betterImgIO = fmap (computeUnboxedP' . collapseColorChannel) imgIO
    in
        betterImgIO
ripTile :: Array U DIM2 (Word8,Word8,Word8,Word8) -> Int -> Tilearr
ripTile tilemap loc = 
    let (xcoord,ycoord) = loc `divMod` 16
        xstart = xcoord*16
        ystart = ycoord*16
    in
    computeUnboxedP' $ extract (Z:.xstart:.ystart) (Z:.16:.16) tilemap  

gid2loc :: [Tileset] -> Word32 -> (Int,Int) --which tileset, how far in
gid2loc tss gid = 
    let findTileset (x:[]) passed = (x,passed)
        findTileset (x:y:xs) passed = if (tsInitialGid y) > gid then (x,passed) else findTileset (y:xs) (passed+1)
        (ts,which) = findTileset tss 0
        tsx = iWidth $ head $ tsImages ts
        tsy = iHeight $ head $ tsImages ts --assuming one image per ts, which I have never seen different so far...
    in 
        (which, fromIntegral $ gid - (tsInitialGid ts))

-}

-- These two are used together a lot
--must be efficient so it can be run many times constantly
{-gid2coord :: [Tileset] -> Word32 -> (Int,(Int,Int)) --which tileset in list, then coord
gid2coord tss gid = 
    let which = (subtract 1) . length $ filter (<=gid) (fmap tsInitialGid tss) --blegh. test this for bugs
        ts = tss !! which
        tileWidth = tsTileWidth ts --usually 16
        --tileHeight = 16 -- tsTileHeight ts
        --how do I even do more than one image? will I need that, and why not multiple tilesets for that?
        image = head $ tsImages ts
        imageWidth = iWidth image
        --imageHeight = iHeight image
        imageTilesW = imageWidth `div` tileWidth
        --imageTilesH = imageHeight `div` tileHeight
        (y,x) = (fromIntegral (gid-1)) `divMod` imageTilesW --I have to do (gid-1) here because Tiled IDs start at 1, while this starts at 0
    in
        (which,(x,y))-}
--if this works I will be glad 
--dumb names. needs to be given better ones.

