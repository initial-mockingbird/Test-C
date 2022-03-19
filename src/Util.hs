{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Util where 

import System.IO  
import Control.Monad
import Prelude hiding (lookup)
import Match
import AA
import Foreign.C.Types
import Data.Char

-- Number of turns in the game
turns :: Int
turns = 6

-- Path of the file where are saved the words
dictionary :: String
dictionary = "american-english"

---------------------------------
-- Functions                    |
---------------------------------

validWord :: String -> Bool
validWord str 
    | (length str==5) && all isLetter str  = True
    | otherwise = False
    where
        fAnd :: Bool -> Bool -> Bool
        fAnd x y = x && y
        isLetter :: Char -> Bool
        isLetter = (`elem` ['a' .. 'z'])

fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter validWord 

loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary path = do
    contents <- readFile path
    let lista = fiveLetterWords $ words contents
        diag x = (x,x)
    return $ fromList $ fmap diag lista

yesOrNo :: String -> IO Bool
yesOrNo text = withNoBuffer $ do
    putStr ( text ++ " (y/n) ?" )
    c <- getChar'
    b <- case c of 
        'y' -> putStr " y\n" >>return True
        'n' -> putStr " n\n" >>return False
        _   -> putStr "\n"   >> yesOrNo text
    return b


-----------------------------------------------------------
-- No need to read past here ------------------------------
-----------------------------------------------------------

withNoBuffer :: IO a -> IO a
withNoBuffer ma = do 
    stdinBuffer  <- hGetBuffering stdin 
    stdoutBuffer <- hGetBuffering stdout
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdout False

    a <- ma 

    hSetBuffering stdin stdinBuffer
    hSetBuffering stdout stdoutBuffer
    hSetEcho stdout True
    pure a

-- Multi-platform version of `getChar` which has a fix for a GHC bug with Windows cmd/Powershell
getChar' :: IO Char
getChar' = withNoBuffer $ do  -- just in case, DON'T echo
#ifdef mingw32_HOST_OS
      -- Windows has to do things...
      c <- c_getch
      let c' = chr . fromEnum $ c
      return c'
#else
    -- Linux, Unix, Mac OS X can just use the normal getChar
    getChar
#endif

#ifdef mingw32_HOST_OS  
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#endif