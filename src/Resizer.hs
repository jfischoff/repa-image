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

main = execParser opts >>= run
 
run (Config {..}) = do
  --just try to remove the file (prevents an unlikely race condition)
  let handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
  
  removeFile _outputPath `catch` handleExists
            
  runIL $ do
    (tag, image) <- unImage <$> readImage _inputPath 
    
    let Size oldWidth oldHeight = toSize . listOfShape . extent $ image
        newSize = case _newSize of
             Percent p -> Size (ceiling $ p * fromIntegral oldWidth) 
                               (ceiling $ p * fromIntegral oldHeight)
             SizeOption (Just w) Nothing  -> Size w oldHeight
             SizeOption Nothing  (Just h) -> Size oldWidth h
             SizeOption (Just w) (Just h) -> Size w h
             SizeOption _ _   -> error $ "Need either a height, a width, " ++
                                         "a height and a width, or a percent"

    writeImage _outputPath . tag . runIdentity . computeP . resize image $ newSize

unImage x = case x of
    RGBA y -> (RGBA, y)
    RGB  y -> (RGB,  y)

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

