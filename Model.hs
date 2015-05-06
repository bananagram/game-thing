{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}


-- exporting way too much stuff

module Model (
    W(..),
    EasyMap(..), --GameMap(..), DungeonMap(..),
    ATileset(..),
    Entity(..), EntType(..), -- EntityID, EntityID will just be an Int
    Sprite(..), FrameID(..),
    --AISetting(..), StatChange(..), StatName(..), TrapTile(..),
    Coord, cmult, cadd, cshift, cplace,
    --liftParr, liftTarr, 
    Tilearr, Pixelarr, Colorarr, GreatImage(..), Pixel(..),
    tarr2parr, parr2tarr, parr2carr, pix2col,
    mapImage, moveImage, expandImg, 
    emptyTile, emptyTileP,
    Animation(..), AnimBehavior(..),-- animationMap,
    UI(..), UIMode(..),
    Keyset(..),
    Action(..), Direction(..),
    stepper, eventManager
    --mash, mashMany, mergePixel, withinBounds,
)

where

import Chars
import Shapes

import Data.Tiled
--import Data.Functor
import Control.Applicative
import Data.Monoid hiding (Any,All)
import Data.Array.Repa hiding (map,(++))
import qualified Data.Array.Repa as Repa
import Data.Word
import Graphics.Gloss.Interface.IO.Game
--import qualified Data.IntMap as IM
--import Data.IntMap (IntMap)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Sequence as Sq
import Data.Sequence (Seq)
import Data.List (intersperse)
import Data.Maybe
import Control.Monad.Reader hiding (Any,All)
import Control.Monad.Writer hiding (Any,All)
import Control.Monad.State
import Control.Lens hiding (traverse)
--import System.IO.Unsafe
--import Debug.Trace
-- wtf man, why doesn't this work?
--{-# LANGUAGE TupleSections #-}
--a = (1,)
--b = a 4
--{-#LANGUAGE FlexibleInstances #-}
--instance (Shape sh) => Functor (Array D sh) where
--    fmap = Repa.map


data W = W -- Game state
    { _wTime :: Float -- keeping track of that time here
    , _wTimeI :: Int -- keep track of the number of frames that have passed
    , _wUI :: UI -- contains the UI state
    , _wLoc :: Coord -- gotta keep track of the current location in the map here
    , _wEvents :: Seq Event  --shall be cleared every tick
    , _wKeyset :: Keyset  --shall be updated every time a key changes state
    --, wFloor :: Int
    , _wPlayer :: Entity  -- need to keep it here to keep it easy
    , _wEntities :: [Entity] -- I will store the entities in the world.
    -- I could make entities a Set, and they would be ordered by their numerical ID. just need to make sure I have a good number generation algorithm
    --, wPlayer :: Int -- the ID of the player for ...nah, no convenience yet
    -- Map data
    --_wMaps :: [GameMap],
    , _wMap :: !EasyMap --DungeonMap -- there is only one map at a time for now
    , _wSprites :: !(M.Map String Sprite) --the string is the easier identifier
    , _wFontmap :: !Pixelarr
    , _wWindowSize :: Coord
    --wLayers :: [Layer], --uh, do I put the map here? easier to access it from here, same with entities
    -- Image data
    --wName2tile :: M.Map String Tilearr, --I think. name of tile to array of tile
    --wGid2tile :: M.Map Word32 Tilearr,
    --wTileRefer :: Word32 -> Maybe Tilearr, -- this is in the map. the tileset IDs are unique to each map (unless the maps are made well)
    --wImages :: [(FilePath,Tilearr)] --if an image is needed that's not here, it's loaded and put in. unimplemented
    --wTilesets :: 
} 
-- what if read-only data was put in a Reader thing?



--data DungeonMap = DungeonMap 
--    { dmapTiledMap :: TiledMap
--    , dmapRefer :: Int -> Coord -> Maybe Word32 -- I (might) need to include more in this refer, such as traps
--    --dmapTraps :: [((Int,Int),TrapTile)],
--    , dmapFloors :: Int
--    , dmapInitEntities :: Seq Entity
--    , dmapTileRefer :: Word32 -> Maybe Tilearr
--    , dmapDimensions :: (Int,Int) -- width, height in tiles
--}

--data GameMap = GameMap {
--    gmMap :: Either DungeonMap TiledMap
--    --gmTileMap :: M.Map Word32 Tilearr
--}

data EasyMap = EasyMap 
    { emapGrid :: Map Coord Word32 
    , emapTileset :: ATileset
    , emapTileGrid :: Map Coord Tilearr
    } deriving (Show)

-- new 
--data DungeonMap = DungeonMap 
--    { dmapTiledMap :: Maybe TiledMap
--    , dmapImageData :: MapImage                  -- what about floors though
--    , dmapRefer :: Int -> Coord -> Maybe Word32  -- this is for terrain and such, not just image
--    --, dmapTraps :: [((Int,Int),TrapTile)]
--    , dmapFloors :: Int
--    , dmapInitEntities :: Seq Entity
--    , dmapTileRefer :: Word32 -> Maybe Tilearr
--    , dmapDimensions :: (Int,Int)                 -- width, height in tiles
--}

-- And these shall be the types of map image. 
--data MapImage = 
--    MapFromTiled (Map Coord Tilearr) |
--    MapFromGenerator (Coord -> Tilearr) |
--    MapFromBitmap Tilearr

data ATileset = ATileset
    { tsMorph :: Word32 -> Tilearr } --map from word to image data
instance Show ATileset where
    show _ = "Tileset"

data ATile = ATile
    { atGid :: Word32
    , atArr :: Tilearr
    , atExamineInfo :: String }
instance Show ATile where
    show at = mconcat [show $ atGid at, " info: ", atExamineInfo at]


type Coord = (Int,Int)

--this is stored as four Word8's so it can be unboxed. It's unboxed because WHY NOT, and to differentiate from a Pixelarr, even though I really don't need to.
-- It is a static array that may be used many times. 
type Tilearr = Array U DIM2 Pixel --and always 16x16 usually
type Pixelarr = Array D DIM2 Pixel
-- eh, intermediate type.
type Colorarr = Array D DIM2 Color


data GreatImage = GreatImage 
    { gimLoc :: Coord
    , gimData :: Pixelarr}

type Pixel = (Word8,Word8,Word8,Word8)

--data Animation = Animation
--    { animBehaviorMap :: Map String AnimBehavior
--    , animSprite :: Sprite --the sprite need not be in the 
--    }
type Animation = Map String AnimBehavior


data AnimBehavior = 
    Still FrameID
    -- sequence of frames, each frame going x frames 
    | Changing Int (Seq FrameID) 
    -- length of animation in frames for convenience, and how long until the frame expires and changes
    -- assuming 30 FPS for now
    | Dynamic Int [(Int,FrameID)] deriving (Show)
    -- | Weird Int [(Int,FrameID,AnimFrameBehavior)]

data Entity = Entity
    { _eName :: String --will be used to identify player definitely
    , _eType :: EntType
    , eUID :: UID -- this shall be what identifies each entity. I will map over the entity sequence to get the right entity
    --, eSpecies :: SpeciesName 
    --eStatChanges :: [StatChange],
    , _eSprite :: Sprite --though it can be inferred, better to carry it around
    , _eAnim :: Animation
    --, eAISetting :: AISetting
    -- Temp datas
    , _eMapCoord :: Coord
    , _eDirection :: Direction
    , _eAnimState :: (Int,String) -- the frame number it started at and the behavior it's using
    , _eMovementExp :: Maybe Int -- the frame where the current movement will expire and movement can be regained
}

type UID = Int


data EntType = 
    PlayerEnt
    | StationaryTalkableNPCEnt
    | OtherEnt
    deriving (Show,Eq)


--going to have to hardcode behavior for now
-- Used to create Sprites
data SpriteConstructor = SpriteConstructor {
    spcName :: String,
    spcPath :: FilePath,
    spcFrameCoords :: (FrameID,(Coord,Coord)) --first is pixel displacement, second is 
    --spInitFrame :: Tilearr, --not very useful, just convenient. also hard to construct outside of runtime
    --spFrames :: M.Map FrameID Tilearr, --no, don't put this here or I'll have to construct it at runtime
    --behavior :: String, --uh, map this to the behaviors? wait, this is in entity not sprite
}
--must include data for how it attacks too. what order its frames display in
--frame behavior is encoded in the same way as the animations
data Sprite = Sprite 
    { sprDef :: Tilearr
    , sprFrames :: M.Map FrameID Tilearr --wish I could make this a function, but that isn't showable
} deriving (Show)

type FrameID = String

--data FrameID =
--    BlankFrame
--    | InitFrame
--    | Sleeping1
--    | Sleeping2
--    deriving (Show,Eq,Ord)



-- what would be in the UI? 
-- and no point to rendering the UI before centering the world.
data UI = UI 
    { _uiMode :: UIMode -- if the string doesn't match in the case, it's displayed at the top
    , _uiListPlace :: Int
    , _uiShow :: Bool
    , _uiSampleText :: String
    } deriving (Show)

-- it's perfectly good to store relevant data in the ui mode
data UIMode = Normal 
    -- the id, the id of the location in the dialogue. 
    -- if text loaded slowly it would include the ticks until the loading finished. it could be canceled with a
        -- see, I can plan design here
    | TextMode UID String
    deriving (Show)


data Keyset = Keyset 
    { kPollKeys :: S.Set Key
    , kToggleKeys :: [(KeyState,Key)]
} deriving (Show)
--type Keyset = S.Set Key
-- actions will be hard. does the ation-creating function determine context, or the action-interpreting one, or both?
data Action = 
      NoAction
    | Save
    | Escape
--    | Move EntityID Direction
    | Multiple Action Action
    | ManyActions [Action]
    deriving (Show,Eq,Ord)



data Direction =
    Updir
    | Downdir
    | Leftdir
    | Rightdir
    | RightUp
    | RightDown
    | LeftUp
    | LeftDown
    deriving (Show,Eq,Ord)


type DiaRef = String
type Dialogue = M.Map DiaRef DiaComponent

data DiaComponent =
    DiaText DiaRef String
    | DiaEnd
    -- | DialogueOptions
    deriving (Show)


makeLenses ''W
makeLenses ''UI
makeLenses ''Entity

--------------------------------------------------------------------------------
-- All sorts of types in the world
--------------------------------------------------------------------------------

--class ImageN a where
--    nullImage :: a



--data AISetting = 
--    Following
--    | Friendly
--    | Enemy deriving (Show,Eq,Ord)


-- the second part is the number of changes?
--
--data TrapTile = 
--    FailedTrap deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------
-- Image data
--------------------------------------------------------------------------------



cmult :: Coord -> Coord -> Coord
cmult (kx,ky) (x,y) = (x*kx,y*ky)
--cmult k (x,y) = (x*k,y*k)
cadd :: Coord -> Coord -> Coord
cadd (x0,y0) (x1,y1) = (x0+x1,y0+y1)
cshift :: Coord -> GreatImage -> GreatImage
cshift c1 (GreatImage c ps) = GreatImage (cadd c c1) ps
cplace :: Coord -> GreatImage -> GreatImage
cplace c1 (GreatImage _ ps) = GreatImage c1 ps

tarr2parr :: Tilearr -> Pixelarr
{-# INLINE tarr2parr #-}
tarr2parr = delay
parr2tarr :: Pixelarr -> Tilearr
{-#INLINE parr2tarr #-}
parr2tarr = computeUnboxedS
parr2carr :: Pixelarr -> Colorarr
{-# INLINE parr2carr #-}
parr2carr = Repa.map pix2col
pix2col :: Pixel -> Color
{-# INLINE pix2col #-}
pix2col (r,g,b,a) = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
emptyTile :: Tilearr
emptyTile = computeUnboxedS $ fromFunction (Z:.24:.24) (const (0,0,0,0))
emptyTileP :: Pixelarr
emptyTileP = fromFunction (Z:.24:.24) (const (0,0,0,0))

instance Monoid GreatImage where
    mempty = GreatImage (0,0) (fromFunction (Z:.0:.0::DIM2) (const (0,0,0,0))) -- contains absolutely nothing.
    mappend = mash                          -- ^ actually might grow arrays but the new contents have no effect anyway
    --mappend a b = mashMany [a,b]
    mconcat = mashMany --foldr mash mempty -- mashMany is still broken
instance Show GreatImage where
    show img = 
        let pix = gimData img
            (Z:.xs:.ys) = extent pix
            (xd,yd) = gimLoc img
        in  "Displacement " <> show xd <> " * " <> show yd <> "\n" <>
            (mconcat $ intersperse "\n" $ (\y -> mconcat $ intersperse " " $ fmap (\x -> show $ pix ! (Z:.x:.y)) [0..xs-1]) <$> [0..ys-1])
            --for (y or line) in [0..y]
            --    intersperse " " $ fmap (show $ pix ! (Z:.x:.y)) [0..x]
placeImage :: Coord -> GreatImage -> GreatImage
placeImage c1 img = GreatImage c1 (gimData img)
moveImage :: Coord -> GreatImage -> GreatImage
moveImage c1 (GreatImage c p) = GreatImage (cadd c1 c) p
mapImage :: (Pixelarr -> Pixelarr) -> GreatImage -> GreatImage
mapImage f (GreatImage c p) = GreatImage c (f p)
mapCoord :: (Coord -> Coord) -> GreatImage -> GreatImage
mapCoord f (GreatImage c p) = GreatImage (f c) p
newBlankImage :: Coord -> DIM2 -> GreatImage
newBlankImage c size = GreatImage c (fromFunction size (const (0,0,0,0)))


-- remember to include graphical offset in new ones later
instance Show Entity where
    show e = mconcat ["Name: ", _eName e, " Coord: ", show $ _eMapCoord e, " Direction: ", show $ _eDirection e, 
        " Animations: ", show $ _eAnim e, " Anim state: ", show $ _eAnimState e]
-- if I want to save entities, I'll havbe to make EntitySave and translate to and from to put the sprite back in
--what other kinds of entities would I want?

--------------------------------------------------------------------------------
-- The not-quite-so-important(-yet) stepper function. Will be gargantuan by the time it progresses.
--------------------------------------------------------------------------------






stepper :: Float -> W -> IO W
stepper t w = flip execStateT w $ step1 t

step :: Float -> StateT W IO ()
step time = do
    modify (wTime %~ (+time))
    modify (wTimeI %~ (+1))


step1 :: Float -> StateT W IO ()
step1 time = do --undefined
    -- what would time be besides .0625?
    modify (wTime %~ (+time))
    modify (wTimeI %~ (+1))
    --oh god, input
    (Keyset pollKeys toggleKeys) <- gets _wKeyset
    let toggle True = False
    let toggle False = True
    -- this part doesn't do the right thing if there are multiple j's in the queue
    if (elem (Down,Char 'j') toggleKeys) then
        modify (wUI %~ (uiShow %~ toggle))
        else return ()
    sprites <- gets _wSprites
    time <- gets _wTimeI
    --liftIO $ putStrLn (' ':show time)
    {-
    if time == 4 then do
        liftIO $ putStrLn "Adding character"
        let player = Entity {
            _eName = "player",
            _eType = trace "type evaluated" PlayerEnt,
            _eSprite = maybe defaultSprite id (M.lookup "player" sprites),
            _eAnim = maybe mempty id $ M.lookup "player" animationMap,
            _eMapCoord = trace "coord evaluated" (3,3),
            _eDirection = Updir, --not actually used yet
            _eAnimState = (4,"default"), -- since what time it's been that way, what way it is
            _eMovementExp = Just 4 } -- hah, I won't use that until movements can only happen once every few frames 
        modify (wEntities %~ (player:))
    else return ()
    -}
    gets _wEntities >>= (liftIO . putStrLn) . show . length
    --    modify (wEntities %~ (player:))
    -- this will affect the UI and such too; not limited to modifying the ents
    --modify . (wEntities .~) =<< mapM modEnt =<< mapM modPlayer =<< gets _wEntities 
    modEnts

modEnts = do
    player <- gets _wPlayer
    ents <- gets _wEntities
    ui <- gets _wUI
    case _uiMode ui of
        TextMode uid diaId -> do undefined -- define dialogues
            --keys <- sample "a" []
            --if member keys (Char 'a') then do
            --    let newConv = forwardDia 
        Normal -> do
            keys <- sample "wars" []
            let diff = (if S.member (Char 'a') keys then 1 else 0 + if S.member (Char 's') keys then (-1) else 0, 
                    if S.member (Char 'w') keys then 1 else 0 + if S.member (Char 'r') keys then (-1) else 0)
            modify (wPlayer %~ (eMapCoord %~ (cadd diff)))
            liftIO $ putStrLn $ "moved: " <> show diff <> " from " <> show keys





sample :: (MonadState W m) => [Char] -> [SpecialKey] -> m (Set Key)
sample chars specialKeys = do
    polls <- gets (kPollKeys . _wKeyset)
    -- no toggles
    let keys = map Char chars ++ map SpecialKey specialKeys -- now they'll all be Gloss keys
    --let f set key = bool set (S.insert key set) $ S.member key polls
    --foldM f S.empty keys
    return $ S.intersection (S.fromList keys) polls



{-
modPlayer e = 
    if _eType e /= PlayerEnt then return e else do
        ui <- gets _wUI
        time <- gets _wTimeI
        case _uiMode ui of
            TextMode _ _ -> return e
            Normal -> --if _eMovementExp e + 5 >= time 
                --then do return e --sampleKeys
                --else return e -- don't read from input, don't collect keys. 
                do
                    polls <- gets (kPollKeys . _wKeyset)
                    let dir = sampleDirections polls
                    case dir of
                        --Nothing -> return ()
                        --Just 'w' -> modify (wEntities %~ (eMapCoord %~ (\(x,y) -> (x,y+1))))
                        --Just 'a' -> modify (wEntities %~ (eMapCoord %~ (\(x,y) -> (x-1,y))))
                        --Just 'r' -> modify (wEntities %~ (eMapCoord %~ (\(x,y) -> (x,y-1))))
                        --Just 's' -> modify (wEntities %~ (eMapCoord %~ (\(x,y) -> (x+1,y))))
                        Nothing -> return e
                        Just (Char 'w') -> return $ (eMapCoord %~ (\(x,y) -> (x,y+1))) e
                        Just (Char 'a') -> return $ (eMapCoord %~ (\(x,y) -> (x-1,y))) e
                        Just (Char 'r') -> return $ (eMapCoord %~ (\(x,y) -> (x,y-1))) e
                        Just (Char 's') -> return $ (eMapCoord %~ (\(x,y) -> (x+1,y))) e
            _ -> return e
    where  
        --sampleKeys = do
            -- this list shows the priority each key has
            -- wait a minute, I'm going to have to implement key toggling somehow for these
            -- so the keyset will hold a set of keys that will be sampled, and a set of keys that will need to be toggled
            --case 
        sampleDirections :: S.Set Key -> Maybe Key
        sampleDirections keys = -- so how do I refer and what do I refer to
            case msum $ map (\x -> if S.member x keys then Just x else Nothing) $ map Char ['w','a','r','s'] of
                Nothing -> Nothing
                Just a -> Just a

modEnt e = do
    ui <- gets _wUI
    case _eType e of
        PlayerEnt -> return e
        _ -> case _uiMode ui of
            TextMode _ _ -> return e
            Normal -> return e -- depends what type of ent it is here for what it does
            _ -> return e 
            -- this will make them move every so often and such

-}








eventManager :: Event -> W -> IO W
eventManager e w = execStateT (eventManage e) w

-- only use the keys, because mouse movements and window resizes aren't that important
eventManage :: Event -> StateT W IO ()
eventManage e = do
    case e of
        EventKey key keyState mods location -> do 
            (Keyset pollKeys toggleKeys) <- gets _wKeyset
            let newpolls = case keyState of
                    Down -> S.insert key pollKeys
                    Up -> S.delete key pollKeys
            let newtoggles = toggleKeys <> [(keyState,key)] -- first inb first out
            time <- gets _wTimeI
            modify (wKeyset .~ (Keyset newpolls newtoggles)) >> liftIO (putStrLn $ "step happens at " <> show time)
        _ -> return ()







--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

a1 = GreatImage (0,0) 
    $ fromFunction (Z:.5:.5) (\(Z:.x':.y') -> 
        let x = fromIntegral x' in let y = fromIntegral y' in (1,x,y,255))

a2 = GreatImage (2,0) 
    $ fromFunction (Z:.5:.5) (\(Z:.x':.y') -> 
        let x = fromIntegral x' in let y = fromIntegral y' in (2,x,y,1))

--withArray f (GreatImage c p) = GreatImage c (f p)

--mash = const

mash :: GreatImage -> GreatImage -> GreatImage
mash img1@(GreatImage (xd1,yd1) arr1) img2@(GreatImage (xd2,yd2) arr2) =
    let (Z:.w1:.h1) = extent arr1
        (Z:.w2:.h2) = extent arr2
        --getEncompassingShape ::  -> DIM2
        
        --newLoc = case intersectDim (Z:.xd1:.yd1) (Z:.xd2:.yd2) of (Z:.a:.b) -> (a,b)

        lowx = min xd1 xd2
        lowy = min yd1 yd2
        highx = max (xd1+w1) (xd2+w2)
        highy = max (yd1+h1) (yd2+h2)
        (GreatImage _ expandedArr1) = expandImg (lowx,lowy) (highx,highy) img1
        (GreatImage _ expandedArr2) = expandImg (lowx,lowy) (highx,highy) img2
        --newShape = Z:.highx-lowx:.highy-lowy
        --blankpix (_,_,_,0) = True
        --blankpix _ = False
        --imageRefer (Z:.x:.y) (GreatImage (xd,yd) arr) = arr ! (Z:.x-xd:.y-yd)

        {-
        newimg1 = backpermuteDft 
            (fromFunction (Z:.highx-lowx:.highy-lowy) (const (0,0,0,0)))
            (\sh0@(Z:.x:.y) ->                     -- this is offset of x, plus offset of image, minus offset of result
                let sh1 = Z:.x+xd1-lowx:.y+yd1-lowy in
                if withinBounds sh1 img1 && not (blankpix $ imageRefer sh1 img1) then Just sh1 else Nothing)
            arr1
        mashed = backpermuteDft
            newimg1
            (\sh0@(Z:.x:.y) ->
                let sh1 = Z:.x+xd2-lowx:.y+yd2-lowy in
                if withinBounds sh1 img2 && not (blankpix $ imageRefer sh1 img2) then Just sh1 else Nothing)
            arr2 
        -}
        --newBlankImage (lowx,lowy) (Z:.max())
    in
        GreatImage (lowx,lowy) (Repa.zipWith mergePixel expandedArr1 expandedArr2)
        --GreatImage (lowx,lowy) mashed

--mash :: GreatImage -> GreatImage -> GreatImage
--mash img1@(GreatImage (xd1,yd1) arr1) img2@(GreatImage (xd2,yd2) arr2) =
--    let (Z:.w1:.h1) = extent arr1
--        (Z:.w2:.h2) = extent arr2
--        lowx = min xd1 xd2
--        lowy = min yd1 yd2
--        highx = max (xd1+w1) (xd2+w2)
--        highy = max (yd1+h1) (yd2+h2)
--        (GreatImage _ expandedArr1) = expandImg (lowx,lowy) (highx,highy) img1
--        (GreatImage _ expandedArr2) = expandImg (lowx,lowy) (highx,highy) img2
--    in
--        GreatImage (lowx,lowy) (Repa.zipWith mergePixel expandedArr1 expandedArr2)

--instance Ord DIM2 where
--    compare sh0@(Z:.x0:.y0) sh1@(Z:.x1:.y1) =
--        case compare x0 x1 of
--            GT -> GT
--            LT -> LT
--            EQ -> compare y0 y1 



mashMany :: [GreatImage] -> GreatImage
mashMany bits = 
    let --coords = fmap (\(GreatImage coord _) -> coord) bits
        --extents = fmap (\(GreatImage _ arr) -> extent arr) bits
        pairs = fmap (\(GreatImage c arr) -> (c,extent arr)) bits
        maxesX = fmap (\((xi,yi),(Z:.xa:.ya)) -> xi+xa) pairs --does this even work?
        maxesY = fmap (\((xi,yi),(Z:.xa:.ya)) -> yi+ya) pairs --does this even work?
        largestX = maximum maxesX
        largestY = maximum maxesY
        smallestx = minimum maxesY
        smallesty = minimum maxesY
        -- base x and base y. these are what decide the loc of the result
        bX = minimum $ fmap (\(GreatImage (x,_) _) -> x) bits
        bY = minimum $ fmap (\(GreatImage (_,y) _) -> y) bits
        maxXArr = maximum (fmap (\(_,Z:.x:._) -> x) pairs) -- I don't know which of these is right
        maxYArr = maximum (fmap (\(_,Z:._:.y) -> y) pairs)
        --base = fromFunction (Z:.largestX:.largestY) (const (0,0,0,0))
        -- the base will envelop all, like the sky around all heavently objects. I can refer to it for sizes
        base = GreatImage (bX,bY) $ fromFunction (Z:.largestX:.largestY) (const (0,0,0,0))
        foldingBits = base:reverse bits

        func (GreatImage (xstart,ystart) arr) basearr = 
            let (Z:.xsize:.ysize) = extent arr in
            --backpermuteDft
            --    basearr
            --    (\(Z:.x:.y) -> if within xstart (xstart+xsize) ystart (ystart+ysize) x y && (alpha $ arr ! (Z:.x:.y)) /= 0 then Just (Z:.x:.y) else Nothing)
            --    arr
                traverse arr
                    (const (extent basearr)) --  view _4 (f sh) /= 0 || 
                    (\f sh@(Z:.x:.y) -> if (x >= xstart && x < xstart+xsize && y >= ystart && y < ystart+ysize)
                            && view _4 (f (Z:.x-xstart:.y-ystart)) == 255 
                        then f (Z:.x-xstart:.y-ystart)
                        else basearr ! sh)
        base' = foldr func (gimData base) (reverse bits)
        
        --g (GreatImage (xstart0,ystart0) arr) basearr = 

        {-
        bigFunc (bit:[]) = bit
        bigFunc (bit0:bit1:bit2:bit3:bts) = 
            let (GreatImage disp0@(xstart0,ystart0) arr0) = bit0
                (GreatImage disp1@(xstart1,ystart1) arr1) = bit1
                (GreatImage disp2@(xstart2,ystart2) arr2) = bit2
                (GreatImage disp3@(xstart3,ystart3) arr3) = bit3
                (Z:.xsize0:.ysize0) = extent arr0
                (Z:.xsize1:.ysize1) = extent arr1
                (Z:.xsize2:.ysize2) = extent arr2
                (Z:.xsize3:.ysize3) = extent arr3
                newArr = traverse4 arr0 arr1 arr2 arr3
                    --(const $ const $ let  determineMaxes [bit0,bit1]) --not this again :(
                    -- maybe I'll make it better later
                    (const $ const $ const $ const $ extent $ gimData base)
                    (\f g h i sh@(Z:.x:.y) -> 
                        if (x>=xstart3&&x<xstart3+xsize3&&y>=ystart3&&y<ystart3)
                            && view _4 (f (Z:.x-xstart3:.y-ystart3)) == 255
                        then i (Z:.x-xstart3:.y-ystart3)
                        else
                            if (x>=xstart2&&x<xstart2+xsize2&&y>=ystart2&&y<ystart2)
                                && view _4 (g (Z:.x-xstart2:.y-ystart2)) == 255
                            then h (Z:.x-xstart2:.y-ystart2)
                            else 
                                if (x>=xstart1&&x<xstart1+xsize1&&y>=ystart1&&y<ystart1)
                                    && view _4 (g (Z:.x-xstart1:.y-ystart1)) == 255
                                then g (Z:.x-xstart1:.y-ystart1)
                                else 
                                    if (x>=xstart0&&x<xstart0+xsize0&&y>=ystart0&&y<ystart0)
                                        && view _4 (g (Z:.x-xstart0:.y-ystart0)) == 255
                                    then f (Z:.x-xstart0:.y-ystart0)
                                    else (0,0,0,0))
            in  bigFunc $ GreatImage (bX,bY) newArr : bts
        bigFunc (bit0:bit1:bts) = 
            let (GreatImage disp0@(xstart0,ystart0) arr0) = bit0
                (GreatImage disp1@(xstart1,ystart1) arr1) = bit1
                (Z:.xsize0:.ysize0) = extent arr0
                (Z:.xsize1:.ysize1) = extent arr1
                newArr = traverse2 arr0 arr1
                    --(const $ const $ let  determineMaxes [bit0,bit1]) --not this again :(
                    -- maybe I'll make it better later
                    (const $ const $ extent $ gimData base)
                    (\f g sh@(Z:.x:.y) -> 
                        if (x>=xstart1&&x<xstart1+xsize1&&y>=ystart1&&y<ystart1)
                            && view _4 (f (Z:.x-xstart1:.y-ystart1)) == 255
                        then g (Z:.x-xstart1:.y-ystart1)
                        else
                            if (x>=xstart0&&x<xstart0+xsize0&&y>=ystart0&&y<ystart0)
                                && view _4 (g (Z:.x-xstart0:.y-ystart0)) == 255
                            then f (Z:.x-xstart0:.y-ystart0)
                            else (0,0,0,0))
            in  bigFunc $ GreatImage (bX,bY) newArr : bts


        newImg = bigFunc foldingBits
        -}
    in
        GreatImage (bX,bY) base'
        --newImg


{-
-- the great Associative Binary Operation that makes GreatImage a monoid
mash :: GreatImage -> GreatImage -> GreatImage
mash img1@(GreatImage (xd1,yd1) arr1) img2@(GreatImage (xd2,yd2) arr2) =
    let (Z:.w1:.h1) = extent arr1
        (Z:.w2:.h2) = extent arr2
        --getEncompassingShape ::  -> DIM2
        
        (Z:.a:.b) = intersectDim (Z:.xd1:.yd1) (Z:.xd2:.yd2)
        newLoc = (a,b)

        lowx = min xd1 xd2
        lowy = min yd1 yd2
        highx = max (xd1+w1) (xd2+w2)
        highy = max (yd1+h1) (yd2+h2)
        (GreatImage _ expandedArr1) = expandImg (lowx,lowy) (highx,highy) img1
        (GreatImage _ expandedArr2) = expandImg (lowx,lowy) (highx,highy) img2
        --newShape = Z:.highx-lowx:.highy-lowy
        blankpix (_,_,_,0) = True
        blankpix _ = False
        imageRefer (Z:.x:.y) (GreatImage (xd,yd) arr) = arr ! (Z:.x-xd:.y-yd)
        --newBlankImage (lowx,lowy) (Z:.max())
    in
        GreatImage (lowx,lowy) (Repa.zipWith mergePixel expandedArr1 expandedArr2)
-- it doesn't detect that things are outside the boundary I guess
-- what do I even do? nothing will work
-}

{-
mashIO :: GreatImage -> GreatImage -> IO GreatImage
mashIO img1@(GreatImage (xd1,yd1) arr1) img2@(GreatImage (xd2,yd2) arr2) =
    let (Z:.w1:.h1) = extent arr1
        (Z:.w2:.h2) = extent arr2
        lowx = min xd1 xd2
        lowy = min yd1 yd2
        highx = max (xd1+w1) (xd2+w2)
        highy = max (yd1+h1) (yd2+h2)
        newShape = Z:.highx-lowx:.highy-lowy
        (GreatImage _ expandedArr1) = expandImg (lowx,lowy) (highx,highy) img1
        (GreatImage _ expandedArr2) = expandImg (lowx,lowy) (highx,highy) img2
        blank (_,_,_,0) = True
        blank _ = False
    in
        GreatImage (lowx,lowy) (Repa.zipWith mergePixel expandedArr1 expandedArr2)
-}
alpha :: Pixel -> Word8
{-# INLINE alpha #-}
alpha (_,_,_,x) = x
within :: (Num a, Ord a) => a -> a -> a -> a -> a -> a -> Bool
{-# INLINE within #-}
within x1 x2 y1 y2 x y = x1 <= x && x < x2 && y1 <= y && y < y2
withinBounds :: DIM2 -> GreatImage -> Bool --test this
{-# INLINE withinBounds #-}
withinBounds (Z:.x:.y) (GreatImage (xoff,yoff) arr) =
    let (Z:.xsize:.ysize) = extent arr
    in
        within (xoff) (xsize+xoff) (yoff) (ysize+yoff) x y
-- args: starting coord, ending coord, image to expand
expandImg :: Coord -> Coord -> GreatImage -> GreatImage
{-# INLINE expandImg #-}
{-
expandImg (x1,y1) (x2,y2) img@(GreatImage (xo,yo) arr) = 
    let newShape = Z:.x2-x1:.y2-y1
        newarr = 
            backpermuteDft 
            (fromFunction newShape (const (0,0,0,0)))
            (\(Z:.x:.y) -> if withinBounds (Z:.x+xo-x1:.y+yo-y1) img then Just (Z:.x+xo-x1:.y+yo-y1) else Nothing)
            arr
    in
        GreatImage (x1,y1) newarr
-}
expandImg (x1,y1) (x2,y2) img@(GreatImage (xo,yo) arr) =
    let newShape = Z:.x2-x1:.y2-y1
        (xdiff,ydiff) = (xo-x1,yo-y1)
        (Z:.xs:.ys) = extent arr
        newarr =
            backpermuteDft
            (fromFunction newShape (const (0,0,0,0)))
            (\(Z:.x:.y) -> 
                if all id [x>=xdiff, x<xdiff+xs, y>=ydiff, y<ydiff+ys]
                then Just (Z:.x-xdiff:.y-ydiff)
                else Nothing)
            arr
    in  GreatImage (x1,y1) newarr

--resulting array should be (Z:.x2-x1:.y2-y1)

--very simple right now. no blending yet
mergePixel :: Pixel -> Pixel -> Pixel
{-# INLINE mergePixel #-}
mergePixel p1 (_,_,_,0) = p1
mergePixel _ p2 = p2




