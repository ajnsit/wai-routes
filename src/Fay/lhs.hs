-- Converts .lhs (literary Haskell files) to .hs (plain Haskell files)
-- Keeps only the statements which are normally compiled, plus blank lines.

-- To use:
--    ghc --make lhs2hs.hs
-- to get an executable file lhs2hs.  
-- Then 
--    lhs2hs filename
-- will open filename.lhs and save the converted file in filename.hs

-- by Scot Drysdale on 7/28/07, based on SOE program on p. 241

module Main where
import System.IO
import System.IO.Error
import System.Environment   -- to allow getArgs

-- Opens a file, given name and mode
openGivenFile :: String -> IOMode -> IO Handle
openGivenFile name mode 
  = catch (do handle <- openFile name mode
              return handle)
          (\e -> error ("Cannot open " ++ name))
 
main = do args <- getArgs
          fromHandle <- openGivenFile (args !! 0 ++ ".lhs") ReadMode
          toHandle <- openGivenFile (args !! 0 ++ ".hs") WriteMode
          convertFile fromHandle toHandle
          hClose fromHandle
          hClose toHandle

-- Converts all the lines in a file
convertFile :: Handle -> Handle -> IO ()
convertFile fromHandle toHandle 
  = catch (do line <- hGetLine fromHandle
              case line of
                ('>' : ' ' : rest) -> hPutStrLn toHandle rest
                ('>' : rest)       -> hPutStrLn toHandle rest
                ('\n' : rest)      -> hPutStrLn toHandle line
                ('\r' : rest)      -> hPutStrLn toHandle line
                ""                 -> hPutStrLn toHandle line
                _                  -> hPutStrLn toHandle ('-':'-':' ':line) --return ()
              convertFile fromHandle toHandle)
           (\error -> if isEOFError error then return ()
                                          else ioError error)
       
