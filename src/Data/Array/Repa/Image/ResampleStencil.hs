{-# LANGUAGE FlexibleContexts, RecordWildCards, DeriveFunctor, 
    QuasiQuotes, TemplateHaskell, NoMonomorphismRestriction,
    BangPatterns, MagicHash, PatternGuards, ScopedTypeVariables,
    FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    TypeFamilies, ExplicitForAll, EmptyDataDecls #-}
module Data.Array.Repa.Image.Resample where
import Data.Array.Repa
import Data.Array.Repa.Unsafe
import Data.Word
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import qualified Data.Vector.Unboxed as U
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Array.Repa.Repr.Delayed
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Repr.Partitioned
import Data.Array.Repa.Repr.HintSmall
import Data.Array.Repa.Repr.Undefined
import Data.Array.Repa.Stencil.Base
import Data.Array.Repa.Stencil.Template
import Data.Array.Repa.Stencil.Partition
import GHC.Exts

-- | A index into the flat array.
--   Should be abstract outside the stencil modules.
data Cursor
	= Cursor Int
    
--resize :: Source r Word8 => Array r DIM3 Word8 -> Size -> Array D DIM3 Word8
--resize = undefined

testImage :: Array U DIM2 Double
testImage = fromUnboxed (Z :. 3 :. 3) $ U.fromList [1,2,3,
                                                    4,5,6,
                                                    7,8,9]

bilinearAccum :: (DIM2 -> Maybe Double) -> DIM2 -> Double -> Double -> Double
bilinearAccum getCoeff sh val acc = case getCoeff sh of
    Just x  -> val + acc
    Nothing -> acc

--TODO
--to get this to work i will need to fuck with the guts of map stencil
--Instead of map stencil returning an offset
--it will return and offset and absolute value
--that way I can rescale it
--will have to think about this more
--my guess is that there will be a transform stencil that includes a 

--modify the case statement to work directly on the sh
--depending on what corner perform the 
--    dBottomRight * (1 - x') * (1 - y') + dBottomLeft * x' * (1 - y') +
--    dTopRight    * (1 - x') * y'       + dTopLeft    * x' * y' where
--    
--    y' = y - (fromIntegral . floor $ y)
--    x' = x - (fromIntegral . floor $ x)

bilinearStencil :: Stencil DIM2 Double
bilinearStencil = (makeStencil (Z :. 2 :. 2) coeff)
                     { stencilAcc = bilinearAccum coeff}
                     
coeff ix = case ix of
     Z :.  0 :. 0  -> Just 1
     Z :. -1 :. 0  -> Just 1
     Z :.  0 :. -1 -> Just 1
     Z :. -1 :. -1 -> Just 1
     _ -> Nothing
                         
                            
testMap :: IO ()                             
testMap = do 
    x <- computeP $ mapStencil2 BoundClamp bilinearStencil testImage
    print $ toUnboxed x
    
    -- | Apply a stencil to every element of a 2D array.
