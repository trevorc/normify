module Main where

import Prelude hiding (all)
import Control.Arrow ((&&&), (***), second)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when, unless)
import Data.Tuple (swap)
import Data.Foldable (Foldable(), all)
import System.Directory (doesDirectoryExist, getDirectoryContents,
                         renameDirectory, renameFile)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import GHC.IO.Encoding (getFileSystemEncoding, textEncodingName)
import Data.Text.ICU.Normalize (NormalizationMode(NFC, NFD), isNormalized, normalize)

import qualified Data.Map as M
import qualified Data.Text as T

assertThat :: Show a => (a -> Bool) -> a -> a
assertThat p x = if p x
                   then x
                   else error $ "assertion failed: " ++ show x

normalizations :: NormalizationMode -> [T.Text] -> M.Map T.Text [T.Text]
normalizations mode = M.fromListWith (++) .
                      map (normalize mode &&& return)

hasSingletonValues :: Foldable t => t [a] -> Bool
hasSingletonValues = all $ (== 1) . length

flattenMap :: (Show a, Eq a) => M.Map a [a] -> [(a, a)]
flattenMap = filter (uncurry (/=)) .
             map (second head) .
             M.toList .
             assertThat hasSingletonValues

prompt :: IO Bool
prompt = do
  { putStr "\nContinue [y/N]? "
  ; hFlush stdout
  ; flip elem ["Y", "y"] <$> getLine
  }

detectNormalization :: T.Text -> String
detectNormalization s
    | isNormalized NFC s && isNormalized NFD s = "NFC/NFD"
    | isNormalized NFC s                       = "NFC"
    | isNormalized NFD s                       = "NFD"
    | otherwise                                = "None"

rename :: FilePath -> FilePath -> IO ()
rename src dst = do
  { isDirectory <- doesDirectoryExist src
  ; (if isDirectory
      then renameDirectory
      else renameFile) src dst
  }

canonicalizeDirectory :: NormalizationMode -> FilePath -> IO ()
canonicalizeDirectory mode dir = do
  { renames <- map (T.unpack *** T.unpack) .
               map swap .
               flattenMap .
               normalizations mode .
               map T.pack .
               filter (`notElem` [".", ".."]) <$>
               getDirectoryContents dir
  ; continue <- if null renames
                  then return False
                  else do
                    { forM_ renames $ \(src,_) ->
                          printf "  %s (%s -> %s)\n"
                          src
                          (detectNormalization $ T.pack src)
                          (show mode)
                    ; prompt
                    }
  ; when continue $ mapM_ (uncurry rename) renames
  }

assertUTF8Locale :: IO ()
assertUTF8Locale = do
  { enc <- textEncodingName <$> getFileSystemEncoding
  ; unless (enc == "UTF-8") $
      error $ "invalid filesystem encoding: " ++ enc
  }

main :: IO ()
main = assertUTF8Locale >>
       getArgs >>=
       uncurry canonicalizeDirectory .
       fmap parseDir .
       parseMode
    where { usage = error $ "usage: normify [-n MODE] [DIR]"
          ; parseMode ("-n":"NFD":xs) = (NFD, xs)
          ; parseMode ("-n":"NFC":xs) = (NFC, xs)
          ; parseMode (('-':_):_)     = usage
          ; parseMode xs              = (NFD, xs)
          ; parseDir  []              = "."
          ; parseDir  [dir]           = dir
          ; parseDir  _               = usage
          }
