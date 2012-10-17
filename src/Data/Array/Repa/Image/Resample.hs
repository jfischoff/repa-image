{-# LANGUAGE FlexibleContexts, RecordWildCards, DeriveFunctor #-}
module Data.Array.Repa.Image.Resample where
import Data.Array.Repa
import Data.Word

data Size  = Size Int Int

data Index = Index {
        x :: Double,
        y :: Double
    }
    deriving(Show)

data Neighborhood a = Neighborhood {
        topLeft     :: a,
        topRight    :: a,
        bottomLeft  :: a,
        bottomRight :: a
    }
    deriving(Functor)

resize :: Source r Word8 => Array r DIM3 Word8 -> Size -> Array D DIM3 Word8
resize image newSize@(Size w h) = traverse image resizeShape elemFn where
    elemFn get (Z :. hi :. wi :. p) = bilinear newIndex neighborhood where 
        newIndex@(Index nwi nhi) = remapIndex' $
                                    Index (fromIntegral wi) (fromIntegral hi)
        neighborhood             = fmap (fromIntegral . get) . closestFour $
                                         (Z :. ceiling nhi :. ceiling nwi :. p)
        remapIndex'              = remapIndex inputSize newSize

    resizeShape (Z :. _ :. _ :. x) = Z :. h :. w :. x
    inputSize                      = toSize . listOfShape . extent $ image

bilinear :: Index -> Neighborhood Double -> Word8 
bilinear (Index {..}) (Neighborhood {..}) = ceiling $
    bottomRight * (1 - x') * (1 - y') + bottomLeft * x' * (1 - y') +
    topRight    * (1 - x') * y'       + topLeft    * x' * y' where
    
    y' = y - (fromIntegral . floor $ y)
    x' = x - (fromIntegral . floor $ x)

toSize :: [Int] -> Size
toSize [_, w, h] = Size w h

remapIndex :: Size -> Size -> Index -> Index 
remapIndex (Size oldWidth oldHeight) (Size newWidth newHeight) (Index {..}) = 
    Index ((oldWidth </> newWidth) * x) ((oldHeight </> newHeight) * y) where 
        a </> b = fromIntegral a / fromIntegral b
    
closestFour :: DIM3 -> Neighborhood DIM3
closestFour (Z :. i :. j :. p) = 
    Neighborhood (Z :. i     :. j :. p) (Z :. i     :. j + 1 :. p)
                 (Z :. i + 1 :. j :. p) (Z :. i + 1 :. j + 1 :. p)





