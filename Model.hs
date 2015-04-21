{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Model (
    W(..),
    EasyMap(..), --GameMap(..), DungeonMap(..),
    ATileset(..),
    Entity(..), -- EntityID, EntityID will just be an Int
    Sprite(..), FrameID(..),
    --AISetting(..), StatChange(..), StatName(..), TrapTile(..),
    Coord, cmult, cadd, cshift,
    Renderable, render, renderAt,
    liftParr, liftTarr, 
    renderWorld,
    Tilearr, Pixelarr, Colorarr, GreatImage(..), Pixel(..),
    tarr2parr, parr2tarr, parr2carr, pix2col,
    emptyTile, emptyTileP,
    Animation(..), AnimBehavior,
    UI(..),
    Keyset(..),
    Action(..), Direction(..),
    stepper, eventManager,
    mash, mashMany, mergePixel, withinBounds,
    module Species
)

where

import Chars
import Shapes

import Data.Tiled
--import Data.Functor
import Control.Applicative
import Data.Monoid hiding (Any,All)
import qualified Data.Map as M
import Data.Map (Map)
--import qualified Data.IntMap as IM
--import Data.IntMap (IntMap)
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as Repa
import Data.Word
import Graphics.Gloss.Interface.IO.Game
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
import System.IO.Unsafe
import Debug.Trace
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
    , _wEntities :: [Entity] -- I will store the entities in the world.
    -- I could make entities a Set, and they would be ordered by their numerical ID. just need to make sure I have a good number generation algorithm
    --, wPlayer :: Int -- the ID of the player for ...nah, no convenience yet
    -- Map data
    --_wMaps :: [GameMap],
    , _wMap :: !EasyMap --DungeonMap -- there is only one map at a time for now
    , _wSprites :: !(M.Map String Sprite) --the string is the easier identifier
    , _wFontmap :: !Pixelarr
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
    --, eID :: Int -- this shall be what identifies each entity. I will map over the entity sequence to get the right entity
    --, eSpecies :: SpeciesName 
    --eStatChanges :: [StatChange],
    , _eSprite :: Sprite --though it can be inferred, better to carry it around
    , _eAnim :: Animation
    --, eAISetting :: AISetting
    -- Temp datas
    , _eMapCoord :: Coord
    , _eDirection :: Direction
    , _eAnimState :: (Int,String)
    , _eMovementExp :: Int
}


data EntType = 
    PlayerEnt
    | StationaryTalkableNPCEnt
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
    { _uiMode :: String -- if the string doesn't match in the case, it's displayed at the top
    , _uiListPlace :: Int
    , _uiShow :: Bool
    , _uiSampleText :: String
    } deriving (Show)


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

-- This is a reader for every element that requires access to the world bank of image data.
-- Note that I had to turn on FlexibleContexts to be able to use this...failed as just Reader and with MonadReader r m.
class Renderable a where
    -- here, b doesn't matter but it cares about b anyway...
    -- if I made them write their contents wherever rather than return it, I couldn't return anything to be processed otherwise...
    -- if I kept the option to tell or return open, I would have to return mempty a lot and it might be inconsistent...
    -- second option is the only option...
    -- it is a little inconsistent but it works fine.
    -- note that I should keep the Writer to renderWorld and make render only return image data
    render :: (MonadReader W m) => {-forall b.-} a -> m GreatImage
    renderAt :: (MonadReader W m) => {-forall b.-} Coord -> a -> m GreatImage
    renderAt c1 a = render a >>= return . moveImage c1
    --render :: (Monad m) => a -> ReaderT W m GreatImage
    --renderAt :: MonadReader W m => Coord -> a -> m GreatImage

liftParr :: Pixelarr -> GreatImage
liftParr = GreatImage (0,0)
liftTarr :: Tilearr -> GreatImage
liftTarr = GreatImage (0,0) . tarr2parr

-- Leaving the reader off this for convenience in my rendering function.
imageTarr :: Tilearr -> GreatImage
imageTarr = GreatImage (0,0) . tarr2parr

-- {-
--instance Renderable W where
--    render w = execWriterT $ do

-- maybe a good idea would be to keep everything Renderable as Reader, then use Writer in this function to write the values returned
renderWorld :: W -> IO Pixelarr
renderWorld w = do
    i <- writtenImage
    -- not 00 anb 240,240, something decided by the loc. 
    let i' = expandImg (0,0) (240,240) i
    --i'' <- computeUnboxedP (gimData i') >>= return . delay
    return (gimData i')
    where 
        writtenImage = flip runReaderT w $ execWriterT $
        --do 
            --renderUI
    {- 
            arr <- asks (($ 0) . tsMorph . emapTileset . _wMap)
            tell (imageTarr arr)
            tell (GreatImage (55,0) (delay arr))
    -}
            --asks _wFontmap >>= tell . GreatImage (0,0)
            renderMap >> renderEntities >> renderUI >> extra -- :: (MonadReader W m, MonadWriter GreatImage m) => m GreatImage
            where     
                extra :: WriterT GreatImage (ReaderT W IO) ()
                extra = asks _wTimeI >>= liftIO . print
                renderMap :: WriterT GreatImage (ReaderT W IO) ()
                renderMap = do
                    -- ima write some things
                    m <- asks $ emapTileGrid . _wMap
                    (xl,yl) <- asks _wLoc
                    asks _wTimeI >>= liftIO . print 
                    --liftIO $ print m
                    let f x y = fmap (moveImage (x*24,y*24) . liftTarr) $ M.lookup (x,y) m
                    tell $ mconcat $ catMaybes $ f <$> [-3..3] <*> [-3..3]
                    --tell $ f 0 0
                    --tell $ f 1 1
                renderEntities :: WriterT GreatImage (ReaderT W IO) ()
                renderEntities = do
                    ents <- asks _wEntities
                    datas <- mapM_ render ents -- I believe this tells them
                    --mapM (render >>= tell) ents
                    return ()
                renderUI :: WriterT GreatImage (ReaderT W IO) ()
                renderUI = asks _wUI >>= render >>= tell
{-
-}


cmult :: Coord -> Coord -> Coord
cmult (kx,ky) (x,y) = (x*kx,y*ky)
--cmult k (x,y) = (x*k,y*k)
cadd :: Coord -> Coord -> Coord
cadd (x0,y0) (x1,y1) = (x0+x1,y0+y1)
cshift :: Coord -> GreatImage -> GreatImage
cshift c1 (GreatImage c ps) = GreatImage (cadd c c1) ps
cmove :: Coord -> GreatImage -> GreatImage
cmove c1 (GreatImage _ ps) = GreatImage c1 ps

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
instance Renderable GreatImage where
    render = return -- just write them with tell
placeImage :: Coord -> GreatImage -> GreatImage
placeImage c1 img = GreatImage c1 (gimData img)
moveImage :: Coord -> GreatImage -> GreatImage
moveImage c1 (GreatImage c p) = GreatImage (cadd c1 c) p
mapImage :: (Pixelarr -> Pixelarr) -> GreatImage -> GreatImage
mapImage f (GreatImage c p) = GreatImage c (f p)
newBlankImage :: Coord -> DIM2 -> GreatImage
newBlankImage c size = GreatImage c (fromFunction size (const (0,0,0,0)))


-- remember to include graphical offset in new ones later
instance Show Entity where
    show e = mconcat ["Name: ", _eName e, " Coord: ", show $ _eMapCoord e, " Direction: ", show $ _eDirection e, 
        " Animations: ", show $ _eAnim e, " Anim state: ", show $ _eAnimState e]
-- if I want to save entities, I'll havbe to make EntitySave and translate to and from to put the sprite back in
--what other kinds of entities would I want?

instance Renderable Entity where
    --render = (\x -> render x >>= tell >> return mempty) . _eAnimation
    render e = do
        time <- asks _wTimeI
        let (framestamp,name) = _eAnimState e
        -- the name refers to a specific AnimBehavior
        let framesRunning = time - framestamp
        let spr = _eSprite e
        let computeBeha (Still frameid) = frameid           -- I believe this part will work
        let computeBeha (Changing len frames) = let diff = mod (div framesRunning len) (Sq.length frames) in Sq.index frames diff
        let computeBeha (Dynamic len lst) = "BlankFrame" -- chickening out
        -- the above function computes an animhehavior. the below one chooses one
        let frameid =  maybe "BlankFrame" computeBeha $ M.lookup name (_eAnim e)
        let frame = maybe (sprDef spr) id $ M.lookup frameid (sprFrames spr) 
        let dis = cmult (24,24) $ _eMapCoord e
        return $ GreatImage dis (delay frame)
-- change the world time to number of frames, maybe put another time with seconds


-- RENDERUI RENDUI
instance Renderable UI where
    render ui = do --return mempty --chickened out
        let mode = "displayText" -- _uiMode ui
        loc@(xl,yl) <- asks _wLoc
        --charmap <- asks _wFontmap
        asks _wTimeI >>= renderOneLineTextBox (Z:.40) . show
        --case mode of
        --    --"list1" -> write a box, some entries, and a selection rectangle at the specified point in the UI
        --    -- import these things from Shapes.
        --    "displayText" -> {-return . placeImage (xl+4,yl+40) =<< -}renderOneLineTextBox (Z:.230) "1234567890123456789012345678901234567890"
        --    "" -> return mempty
        --    a -> renderAt (cadd (10,10) loc) a
    --render ui = do
    --    if _uiShow ui then do
    --        tell $ GreatImage (0,0) $ fromFunction (Z:.21:.21) $ (\(Z:.x:.y) -> if x==0 || x==20 || y==0 || y==20 then (255,255,255,255) else (0,0,0,0))
    --        tell $ GreatImage (0,0) $ fromFunction (Z:.40:.40) $ (\(Z:.x:.y) -> if x==5 || x==30 || y==5 || y==30 then (255,255,255,255) else (0,0,0,0))
    --        tell $ GreatImage (0,0) $ fromFunction (Z:.50:.50) $ (\(Z:.x:.y) -> if x==16 || x==46 || y==16 || y==46 then (255,255,255,255) else (0,0,0,0))
    --    else return ()

--renderTextBox :: MonadReader W m => Int -> DIM1 -> String -> m GreatImage
--renderTextBox sh@(Z:.x:.y) msg = 
--    let background = GreatImage (0,0) $ rect sh
--        -- hmm, how do I determine how many characters fit?
    
--    let text = undefined
--    background <> text

renderTwoLineTextBox :: MonadReader W m => DIM1 -> String -> m GreatImage
renderTwoLineTextBox sh@(Z:.x) string = do
    let background = GreatImage (0,0) $ rect (sh:.20)
    --let string1 = if length string > 
    text <- return . mapImage (extract (Z:.0:.0) (Z:.x-2:.20-2)) =<< render string
    text1 <- render string
    return $ background <> text

-- shape is the x length. the height is 20, with one pixel of buffer on either side of the text
renderOneLineTextBox :: MonadReader W m => DIM1 -> String -> m GreatImage
renderOneLineTextBox sh@(Z:.x) string = do
    let background = GreatImage (0,0) $ rect (sh:.20)
    text <- return . mapImage (extract (Z:.0:.0) (Z:.x-2:.20-2)) =<< render string
    text1 <- render string
    return $ printThing "a" "a"
    return $ background <> text

instance Renderable [Char] where
    render str = {- execWriterT $ -} -- maybe if I leave the Writer in there it'll be processed normally?
        foldM (\(diff,img) char -> render char >>= \x -> return (diff+1,img <> cshift (9*diff,0) x)) (0,mempty) str >>= return . snd

instance Renderable Char where
    render char = do -- return mempty --this is where I actually chicken out
        let (x,y) = (9,16) `cmult` (maybe (0,2) id $ M.lookup char charTable)
        return $ printThing " " $ show $ M.lookup char charTable
        img <- asks _wFontmap
        return $ GreatImage (0,0) $ extract (Z:.x:.y) (Z:.9:.16) img
printThing :: Show b => String -> b -> String
printThing a = unsafePerformIO . (\x -> putStrLn x >> return a) . (a<>) . show
{-# NOINLINE printThing #-}


--------------------------------------------------------------------------------
-- Events
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
            _eMovementExp = 4 } -- hah, I won't use that until movements can only happen once every few frames 
        modify (wEntities %~ (player:))
    else return ()
    gets _wEntities >>= (liftIO . putStrLn) . show . length
    --    modify (wEntities %~ (player:))
    -- this will affect the UI and such too; not limited to modifying the ents
    modify . (wEntities .~) =<< mapM modEnt =<< mapM modPlayer =<< gets _wEntities 


modPlayer e = 
    if _eType e /= PlayerEnt then return e else do
        ui <- gets _wUI
        time <- gets _wTimeI
        case _uiMode ui of
            "textmode" -> return e
            "normal" -> --if _eMovementExp e + 5 >= time 
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
            "textmode" -> return e
            "normal" -> return e -- depends what type of ent it is here for what it does
            _ -> return e 
            -- this will make them move every so often and such









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






-- Some bulky things that would be a little inconvenient to put in another file

animationMap = M.fromList $
    ("player", player)
    :("dot", dot)
    :[]

player :: Animation
player = M.fromList [("default",Changing 4 (Sq.fromList ["f1,f2,f1,f3"]))]

dot :: Animation
dot = M.fromList [("default",Still "get the default frame, doofus")]



defaultSprite :: Sprite
defaultSprite = Sprite emptyTile mempty



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
--mashMany = undefined
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
        base = fromFunction (Z:.largestX:.largestY) (const (0,0,0,0))
        f (GreatImage (xstart,ystart) arr) basearr = 
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
        base' = foldr f base (reverse bits)
    in
        GreatImage (bX,bY) base'


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




