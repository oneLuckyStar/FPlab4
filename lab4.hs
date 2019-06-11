import System.IO( Handle, FilePath, IOMode( ReadMode ), 
                  openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr );
import Data.Char;
import Control.Monad( when );
lowerString str = [ toLower loweredString | loweredString <- str];
toUpperCase str = map toUpper str;
dumpFile :: Handle -> FilePath -> Integer ->String-> IO ()
dumpFile handle filename lineNumber k   = do      
    end <- hIsEOF handle
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line ch1 = if ("'"++ch1++"'")==show(head(line::[Char])) then line else  "net";
        putStrLn $ filename ++ ":" ++ show lineNumber ++ ": " ++"  " ++line
        dumpFile (handle) (filename) ( lineNumber + 1) (k);

dumpFile1 handle filename lineNumber k   = do     
    end <- hIsEOF handle
    when ( not end ) $ do
        line <- hGetLine handle
        let line1 line ch1 = if ("'"++ch1++"'")==show(head(line::[Char])) then line else  "";
        putStrLn $ filename ++ ":" ++ show lineNumber ++ ":   " ++ line1 (line) (k)
        dumpFile1 (handle) (filename) ( lineNumber + 1) (k);



main = do{
    hPutStr stderr "Введите название файла: ";
    filename <- getLine;
    handle <- openFile filename ReadMode;     
    putStrLn("Введите символ");
    k<-getLine;
    dumpFile handle filename 1 k;
    hClose handle;
    handle <- openFile filename ReadMode; 
    dumpFile1 handle filename 1 k;
    hClose handle;
}