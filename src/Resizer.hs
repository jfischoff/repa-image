{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Main where
import Data.Array.Repa.Image
import System.Environment
import Data.Word
import Data.Array.Repa hiding ((++), map)
import Data.Array.Repa.IO.DevIL
import Control.Monad
import System.Directory
import Control.Applicative
import Control.Exception(catch, throwIO)
import System.IO.Error(isDoesNotExistError)
import Options.Applicative
import Control.Monad.Identity(Identity, runIdentity)
import Data.Array.Repa.Repr.ForeignPtr
import Foreign.Storable.Tuple
import Foreign.ForeignPtr

main = execParser opts >>= run
 
run (Config {..}) = do
  --just try to remove the file (prevents an unlikely race condition)
  let handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
  
  removeFile _outputPath `catch` handleExists
            
  runIL $ do
    image <- readImage _inputPath 
    
    let [_, oldWidth, oldHeight] = listOfShape . extent . unImage $ image
        newSize = case _newSize of
             Percent p -> Size (ceiling $ p * fromIntegral oldWidth) 
                               (ceiling $ p * fromIntegral oldHeight)
             SizeOption (Just w) Nothing  -> Size w oldHeight
             SizeOption Nothing  (Just h) -> Size oldWidth h
             SizeOption (Just w) (Just h) -> Size w h
             SizeOption _ _   -> error $ "Need either a height, a width, " ++
                                         "a height and a width, or a percent"
                                         
    let newImage = 
          case image of
             RGB x -> RGB . unpackRGB . runIdentity . computeP . resizeRGB (packRGB x) $ newSize

    writeImage _outputPath newImage

packRGB :: Array F DIM3 Word8  -> Array F DIM2 RGBPixelWord8 
packRGB x = result where
    result    = fromForeignPtr (newExtent $ extent x) pixelPtr  
    pixelPtr  = castForeignPtr $ toForeignPtr x :: ForeignPtr RGBPixelWord8
    newExtent (Z :. x :. y :. _) = Z :. x :. y 

unpackRGB :: Array F DIM2 RGBPixelWord8 -> Array F DIM3 Word8 
unpackRGB x = result where
    result    = fromForeignPtr (newExtent $ extent x) pixelPtr  
    pixelPtr  = castForeignPtr $ toForeignPtr x :: ForeignPtr Word8
    newExtent (Z :. x :. y) = Z :. x :. y :. 3

unImage :: Image -> Array F DIM3 Word8
unImage x = case x of
    RGB y -> y
    RGBA y -> y 


opts = info pConfig 
    (fullDesc  
    & progDesc "Resize an image with a HEIGHT and WIDTH or a PERCENT"
    & header   "resizer - a simple image resizing app")
    
data Config = Config {
        _outputPath :: FilePath,
        _newSize    :: Resize,
        _inputPath  :: FilePath
    }
    deriving(Show, Eq)
    
pConfig :: Parser Config
pConfig = Config
    <$> strOption 
        (   long "output"
          & short 'o'
          & metavar "OUTPUT"
          & help "Output file path")
    <*> pResize
    <*> argument str (metavar "INPUT FILE")

data Resize = SizeOption (Maybe Int) (Maybe Int)
            | Percent Double
            deriving(Eq, Show)
    
pResize :: Parser Resize
pResize = pSizeOption <|> pPercent 
    
pSizeOption :: Parser Resize
pSizeOption = SizeOption 
           <$> option
               (   long "width" 
                 & short 'w'
                 & metavar "WIDTH"
                 & help "The ouput width")
           <*> option 
               (   long "height"
                 & short 'h'
                 & metavar "HEIGHT"
                 & help "The output height") 
                 
pPercent :: Parser Resize
pPercent = Percent 
        <$> option 
            (   long "percent"
              & short 'p'
              & metavar "PERCENT"
              & help "The amount to reduce the image as a percent")

