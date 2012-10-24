{-# LANGUAGE FlexibleContexts, RecordWildCards, DeriveFunctor #-}
module Data.Array.Repa.Image.Resample where
import Data.Array.Repa
import Data.Array.Repa.Unsafe
import Data.Word
--import Data.Array.Repa.Specialised.Dim2

--TODO first change the image to use 

data Size  = Size !Int !Int

data Index = Index {
        x :: !Float,
        y :: !Float
    }
    deriving(Show)

data DIM3Neighborhood = DIM3Neighborhood {
        iTopLeft     :: !DIM3,
        iTopRight    :: !DIM3,
        iBottomLeft  :: !DIM3,
        iBottomRight :: !DIM3
    }

data DoubleNeighborhood = DoubleNeighborhood {
        dTopLeft     :: !Float,
        dTopRight    :: !Float,
        dBottomLeft  :: !Float,
        dBottomRight :: !Float
    }

toDouble :: (DIM3 -> Word8) -> DIM3Neighborhood -> DoubleNeighborhood
toDouble f (DIM3Neighborhood {..}) = DoubleNeighborhood 
    (fromIntegral (f (iTopLeft    ))) 
    (fromIntegral (f (iTopRight   ))) 
    (fromIntegral (f (iBottomLeft )))
    (fromIntegral (f (iBottomRight)))
{-# INLINE toDouble #-}

resize :: Source r Word8 => Array r DIM3 Word8 -> Size -> Array D DIM3 Word8
resize image newSize@(Size w h) = traverse image resizeShape elemFn where
    elemFn get (Z :. hi :. wi :. p) = bilinear newIndex neighborhood where 
        newIndex@(Index nwi nhi) = remapIndex oldW oldH w h (fromIntegral wi) (fromIntegral hi)
        neighborhood             = toDouble get $ closestFour
                                         (Z :. ceiling nhi :. ceiling nwi :. p)
    {-# INLINE resizeShape #-}
    resizeShape (Z :. _ :. _ :. x) = Z :. h :. w :. x
    [_, oldW, oldH]                = listOfShape $ extent image

{-# INLINE remapIndex #-}    
remapIndex :: Int -> Int -> Int -> Int -> Float -> Float -> Index 
remapIndex oldW oldH w h x y = 
    Index ((oldW </> w) * x) ((oldH </> h) * y) where 

        {-# INLINE (</>) #-}
        a </> b = fromIntegral a / fromIntegral b
    
{-# INLINE resize #-}

bilinear :: Index -> DoubleNeighborhood -> Word8 
bilinear (Index {..}) (DoubleNeighborhood {..}) = ceiling $
    dBottomRight * (1 - x') * (1 - y') + dBottomLeft * x' * (1 - y') +
    dTopRight    * (1 - x') * y'       + dTopLeft    * x' * y' where
    
    y' = y - (fromIntegral $ floor y)
    x' = x - (fromIntegral $ floor x)
{-# INLINE bilinear #-}

toSize :: [Int] -> Size
toSize [_, w, h] = Size w h
{-# INLINE toSize #-}

    
closestFour :: DIM3 -> DIM3Neighborhood 
closestFour (Z :. i :. j :. p) = 
    DIM3Neighborhood (Z :. i     :. j :. p) (Z :. i     :. j + 1 :. p)
                 (Z :. i + 1 :. j :. p) (Z :. i + 1 :. j + 1 :. p)
{-# INLINE closestFour #-}




