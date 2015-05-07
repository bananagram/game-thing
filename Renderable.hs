{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}



-- This file now holds all the rendering data. Now I don't have to recompile Model until
-- I make changes to the world type.



module Renderable (
    Renderable,
    renderWorld
) where



import Data.Tiled
--import Data.Functor
import Control.Applicative
import Data.Monoid hiding (Any,All)
import Data.Array.Repa hiding (map,(++))
import qualified Data.Array.Repa as Repa
import Data.Word
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M
--import qualified Data.IntMap as IM
--import Data.IntMap (IntMap)
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

import Model
import Chars
import Shapes



------------------------------------------------------------------------------
-- RenderWorld, the important function of rendering
------------------------------------------------------------------------------

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
            --renderMap >>
            --    renderEntities >> 
                renderUI >> 
                extra -- :: (MonadReader W m, MonadWriter GreatImage m) => m GreatImage
            where     
                extra :: WriterT GreatImage (ReaderT W IO) ()
                extra = asks _wTimeI >>= liftIO . print
                renderMap :: WriterT GreatImage (ReaderT W IO) ()
                renderMap = do
                    -- ima write some things
                    m <- asks $ emapTileGrid . _wMap
                    (xl,yl) <- asks _wLoc
                    (xwin,ywin) <- asks _wWindowSize
                    --asks _wTimeI >>= liftIO . print 
                    --liftIO $ print m
                    let f x y = fmap (moveImage ((x-xl+xwin `div` 2)*24,(y-yl+ywin `div` 2)*24) . liftTarr) $ M.lookup (x,y) m
                    tell $ mconcat $ catMaybes $ f <$> [0..8] <*> [0..8]
                    --tell $ f 0 0
                    --tell $ f 1 1
                renderEntities :: WriterT GreatImage (ReaderT W IO) ()
                renderEntities = do
                    ents <- asks _wEntities
                    player <- asks _wPlayer
                    mapM (\e -> render e >>= tell) ents -- I believe this tells them
                    render player >>= tell
                    --mapM (render >>= tell) ents
                renderUI :: WriterT GreatImage (ReaderT W IO) ()
                -- rendering the UI will generate an uncentered image. I must adjust it here. don't know if this is well-adjusted.
                renderUI = do
                    (xl,yl) <- asks _wLoc
                    (xwin,ywin) <- asks _wWindowSize
                    rendered <- asks _wUI >>= render
                    tell $ cshift (xwin `div` 2 + xl*24, ywin `div` 2 + yl*24) rendered





instance Renderable GreatImage where
    render = return -- just write them with tell

-- renderentity rendent
instance Renderable Entity where
    --render = (\x -> render x >>= tell >> return mempty) . _eAnimation
    render e = do
        time <- asks _wTimeI
        let (framestamp,name) = _eAnimState e
        -- the name refers to a specific AnimBehavior
        let framesRunning = time - framestamp
        let spr = argh -- _eSprite e
        let computeBeha (Still frameid) = frameid           -- I believe this part will work
        let computeBeha (Changing len frames) = let diff = mod (div framesRunning len) (Sq.length frames) in Sq.index frames diff
        let computeBeha (Dynamic len lst) = "BlankFrame" -- chickening out
        -- the above function computes an animhehavior. the below one chooses one
        let frameid =  maybe "BlankFrame" computeBeha $ M.lookup name (_eAnim e)
        let frame = maybe (sprDef spr) id $ M.lookup frameid (sprFrames spr) 
        let disp = cmult (24,24) $ _eMapCoord e
        return $ GreatImage disp (delay frame)
-- change the world time to number of frames, maybe put another time with seconds


-- RENDERUI RENDUI
instance Renderable UI where
    render ui = do --return mempty --chickened out
        let mode = Normal -- _uiMode ui
        loc@(xl,yl) <- asks _wLoc
        --charmap <- asks _wFontmap
        time <- asks _wTimeI 
        box <- renderOneLineTextBox (Z:.230) $ "This is a test"-- of generat" --("Time: "<>) $ show time
        --box1 <- renderOneLineTextBox (Z:.230) $ "ing text!"
        --otherText <- render "Mode: 3.14"
        --timeText <- renderAt (0,20) $ "Frame: " <> show time
        --newText <- render "Hello world"
        

        return $ cshift (0,180) box 
            {-<> cshift (0,200) box1 -}
            -- <> otherText 
            -- <> timeText
            -- <> cshift (120,40) newtext 
            -- <> cshift (0,20) frame
        
        --a <- render "Hello world!"
        --b <- render "Lorem ipsum dolor sit amet"
        --c <- render ", consectetur adipiscing "
        --d <- render "elit, sed do eiusmod tempor"
        --e <- render "incididunt ut labore et"
        --f <- render "dolore magna aliqua. Ut "
        --g <- render "enim ad minim veniam, quis"
        --h <- render "nostrudexercitation ullamco" 
        --i <- render "laboris nisi ut aliquip ex"
        --j <- render "ea commodo consequat. "
        --return $ mconcat [a, cshift (0,20) b, cshift (0,40) c, cshift (0,60) d, cshift (00,80) e, cshift (0,100) f, cshift (0,120) g, cshift (0,140) h, cshift (0,160) i, cshift (0,180) h]

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


-- This is a reader for every element that requires access to 
-- the world bank of image data.
-- Note that I had to turn on FlexibleContexts to be able to use this...           failed as just Reader and with MonadReader r m.
class Renderable a where
    render :: (MonadReader W m, MonadIO m) => a -> m GreatImage
    renderAt :: (MonadReader W m, MonadIO m) => Coord -> a -> m GreatImage
    renderAt c1 a = render a >>= return . moveImage c1
    -- it is a little inconsistent but it works fine.
    -- note that I should keep the Writer to renderWorld and make render only return image data
    --render :: (Monad m) => a -> ReaderT W m GreatImage
    --renderAt :: MonadReader W m => Coord -> a -> m GreatImage

liftParr :: Pixelarr -> GreatImage
liftParr = GreatImage (0,0)
liftTarr :: Tilearr -> GreatImage
liftTarr = GreatImage (0,0) . tarr2parr

-- Leaving the reader off this for convenience in my rendering function.
imageTarr :: Tilearr -> GreatImage
imageTarr = GreatImage (0,0) . tarr2parr


--renderTextBox :: MonadReader W m => Int -> DIM1 -> String -> m GreatImage
--renderTextBox sh@(Z:.x:.y) msg = 
--    let background = GreatImage (0,0) $ rect sh
--        -- hmm, how do I determine how many characters fit?
    
--    let text = undefined
--    background <> text

renderTwoLineTextBox :: (MonadReader W m, MonadIO m) => DIM1 -> String -> m GreatImage
renderTwoLineTextBox sh@(Z:.x) string = do
    let background = GreatImage (0,0) $ rect (sh:.20)
    --let string1 = if length string > 
    text <- return . mapImage (extract (Z:.0:.0) (Z:.x-2:.20-2)) =<< render string
    text1 <- render string
    return $ background <> text

-- shape is the x length. the height is 20, with one pixel of buffer on either side of the text
renderOneLineTextBox :: (MonadReader W m, MonadIO m) => DIM1 -> String -> m GreatImage
renderOneLineTextBox sh@(Z:.x) string = do
    let background = GreatImage (0,0) $ rect (sh:.20)
    text <- return . cshift (1,1) . mapImage (extract (Z:.0:.0) (Z:.x-2:.20-2)) =<< render string
    text1 <- render string
    liftIO $ print "a"
    --return $ printThing "a" "a"
    return $ background <> text

instance Renderable [Char] where
    render str = do {- execWriterT $ -} -- maybe if I leave the Writer in there it'll be processed normally?
        --liftIO $ putStrLn $ "Rendered string: " <> str
        foldM (\(diff,img) char -> render char >>= \x -> return (diff+1,img <> cshift (9*diff,0) x)) (0,mempty) str >>= return . snd

instance Renderable Char where
    render char = do
        let (x,y) = (9,16) `cmult` (maybe (0,2) id $ M.lookup char charTable)
        img <- asks _wFontmap
        return $ GreatImage (0,0) $ extract (Z:.x:.y) (Z:.9:.16) img







-- Some bulky things that would be a little inconvenient to put in another file
-- If I split this file in two, this will go in the second file. 

-- Use this to store all the Animations I create
animationMap :: Map String Animation
animationMap = M.fromList $
    ("player", player)
    :("dot", dot)
    :[]

-- this uses the "player" sprite
player :: Animation
player = M.fromList 
    [("default",Changing 4 (Sq.fromList ["f1,f2,f1,f3"]))
    ,("still",Still "f1")]

dot :: Animation
dot = M.fromList 
    [("default",Still "get the default frame, doofus")]

argh :: Sprite
argh = Sprite (computeUnboxedS $ fromFunction (Z:.200:.200) (const (60,60,60,180))) mempty -- always uses the giant, obnoxious default

defaultSprite :: Sprite
defaultSprite = Sprite emptyTile mempty




