
import Graphics.Gloss.Interface.IO.Game
--import Graphics.Gloss.Raster.Field
import Graphics.Gloss.Juicy
import Graphics.Gloss
import Codec.Picture
import qualified Data.Array.Repa as R
import Data.Array.Repa -- ((!),(:.),Z)
import qualified Data.Map as M

import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Monoid

import Model
import Loadmap




main :: IO ()
main = do
    map <- loadEasyMap
    --let (Just tile) = M.lookup (0,0) (emapTileGrid map)
    --let coloredTile = R.map pix2col $ tarr2parr tile
    fontmap <- loadFontmap
    sprites <- loadSprites

    let initUI = UI {
          _uiMode = ""
        , _uiListPlace = 0
        , _uiShow = True
        , _uiSampleText = "Some Text Here"
        }

    let initWorld = W {
          _wTime = 0
        , _wTimeI = 0
        , _wUI = initUI
        , _wLoc = (0,0)
        , _wEvents = mempty
        , _wKeyset = Keyset mempty mempty
        , _wEntities = mempty
        , _wMap = map
        , _wSprites = sprites
        , _wFontmap = delay fontmap}
    
    --let tick = execStateT tickFunction
    let window = InWindow "" (240,240) (400,400)
    playIO
        window
        black
        1
        initWorld
        (\w -> 
            let --(GreatImage (xd,yd) arr) = runReader (render w) w 
                --(xd,yd) = (0,0) 
                --arr = delay tile
            in do
                arr <- renderWorld w -- runReaderT (render w) w 
                --(print "a" >>) $
                return
                    $ fromImageRGBA8 
                    $ generateImage (\x y -> case (arr ! (Z:.x:.y)) of (r,g,b,a) -> PixelRGBA8 r g b a) 240 240) 
        --(\w -> return $ makePicture 240 240 1 1 $ \(x,y) -> makeColor (x/160) (0) (0) 1)
        --(\w -> return $ makePicture 24 24 1 1 (\(x,y) -> coloredTile ! (Z:.ro x:.ro y))) -- $ (\(GreatImage (xl,yl) ps) (x,y) -> pix2col $ ps ! (Z:.round x - xl:.round y - yl)) $ runReader (render w) w) --this is funny because it never actually uses the world argument it gets, except for where it passes such things to its components
        eventManager --(\e w -> return w)
        stepper --(\t w -> return w)
    --display 
    --    window
    --    (makeColorI 30 30 30 255)
    --    (makePicture 240 240 1 1 $ \(x,y) -> makeColor (x/160) (0) (0) 1)

ro :: Float -> Int
ro = round

