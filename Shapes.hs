module Shapes (
    rect
    )
where

import Data.Array.Repa
import Data.Word

type Pixelarr = Array D DIM2 Pixel
type Pixel = (Word8,Word8,Word8,Word8)

rect :: DIM2 -> Pixelarr
rect (Z:.xd:.yd) = fromFunction (Z:.xd:.yd) (\(Z:.x:.y) -> if x == 0 || y == 0 || x == xd-1 || y == yd-1 then (255,255,255,255) else (0,0,0,0))

menuBorder :: DIM2 -> Pixelarr
menuBorder = rect

