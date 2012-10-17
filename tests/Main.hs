module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Array.Repa.Image
import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Eval
import Data.Word

main = defaultMain [] 

testA = fromListUnboxed (Z :. 3 :. 3 :. 3 :: DIM3) [0..26 :: Word8]

resizeTest = computeUnboxedS $ resize testA (Size 2 2)

resizeTest1 = runIL $ do
    RGB a <- readImage "tests/2.png" -- must have alpha!
    let b = resize a (Size 480 320)  
    writeImage "tests/testOuput.png" $ RGB $ computeS b