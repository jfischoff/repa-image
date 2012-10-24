{-# LANGUAGE FlexibleContexts, RecordWildCards, DeriveFunctor, BangPatterns,
    MagicHash, UnboxedTuples #-}
module Data.Array.Repa.Image.Resample where
import Data.Array.Repa
import Data.Array.Repa.Unsafe
import Data.Word
import Data.Array.Repa.Specialised.Dim2

--TODO first change the image to use 

data Size  = Size !Int !Int

data Index = Index {
        x :: !Float,
        y :: !Float
    }
    deriving(Show)

data DIM2Neighborhood = DIM2Neighborhood {
        iTopLeft     :: !DIM2,
        iTopRight    :: !DIM2,
        iBottomLeft  :: !DIM2,
        iBottomRight :: !DIM2
    }

data DoubleNeighborhood = DoubleNeighborhood {
        dTopLeft     :: !RGBPixelDouble,
        dTopRight    :: !RGBPixelDouble,
        dBottomLeft  :: !RGBPixelDouble, 
        dBottomRight :: !RGBPixelDouble 
    }

toDouble :: (DIM2 -> RGBPixelWord8) -> DIM2Neighborhood -> DoubleNeighborhood
toDouble f (!DIM2Neighborhood {..}) = DoubleNeighborhood 
    (fromIntegralT (f (iTopLeft    ))) 
    (fromIntegralT (f (iTopRight   ))) 
    (fromIntegralT (f (iBottomLeft )))
    (fromIntegralT (f (iBottomRight))) where
        
    fromIntegralT (!x, !y, !z) = (fromIntegral x, fromIntegral y, fromIntegral z)
    {-# INLINE fromIntegralT #-}
    
{-# INLINE toDouble #-}

type RGBPixel a = (a, a, a)
type RGBPixelDouble = RGBPixel Float
type RGBPixelWord8  = RGBPixel Word8

{-# INLINE resizeRGB #-}
resizeRGB :: Source r RGBPixelWord8 
       => Array r DIM2 RGBPixelWord8 -> Size -> Array D DIM2 RGBPixelWord8
resizeRGB image newSize@(Size w h) = traverse image resizeShape elemFn where
    elemFn get (Z :. hi :. wi) = bilinear newIndex neighborhood where 
        !newIndex@(Index nwi nhi) = remapIndex oldW oldH w h (fromIntegral wi) (fromIntegral hi)
        !neighborhood             = toDouble get $ closestFour (extent image)
                                         (Z :. ceiling nhi :. ceiling nwi)
    {-# INLINE resizeShape #-}
    resizeShape (Z :. _ :. _) = Z :. h :. w
    ![!oldW, !oldH]           = listOfShape $ extent image

{-# INLINE remapIndex #-}    
remapIndex :: Int -> Int -> Int -> Int -> Float -> Float -> Index 
remapIndex oldW oldH w h x y = 
    Index ((oldW </> w) * x) ((oldH </> h) * y) where 

        {-# INLINE (</>) #-}
        (</>) !a !b = fromIntegral a / fromIntegral b
    

bilinear :: Index -> DoubleNeighborhood -> RGBPixelWord8 
bilinear (!Index {..}) (!DoubleNeighborhood {..}) = floorT $
    dBottomRight <.> ((1 - x') * (1 - y')) <+> dBottomLeft <.> (x' * (1 - y')) <+>
    dTopRight    <.> ((1 - x') * y')       <+> dTopLeft    <.> (x' * y') where
    
    floorT !(!x, !y, !z) = (floor x, floor y, floor z)
    {-# INLINE floorT #-}
    
    (<+>) !(!x0, !y0, !z0) !(!x1, !y1, !z1) = (x0 + x1, y0 + y1, z0 + z1)
    {-# INLINE (<+>) #-}
    
    (<.>) !(!x, !y, !z) !s = (s*x, s*y, s*z) 
    {-# INLINE (<.>) #-}
    
    !y' = y - (fromIntegral $ floor y)
    !x' = x - (fromIntegral $ floor x)
{-# INLINE bilinear #-}

toSize :: [Int] -> Size
toSize [w, h] = Size w h
{-# INLINE toSize #-}

    
closestFour :: DIM2 -> DIM2 -> DIM2Neighborhood 
closestFour !extent (Z :. i :. j) = 
    DIM2Neighborhood (clampToBorder2' (Z :. i     :. j    )) 
                     (clampToBorder2' (Z :. i     :. j + 1))
                     (clampToBorder2' (Z :. i + 1 :. j    )) 
                     (clampToBorder2' (Z :. i + 1 :. j + 1)) where

    {-# INLINE clampToBorder2' #-}
    clampToBorder2' = clampToBorder2 extent

                     
{-# INLINE closestFour #-}




