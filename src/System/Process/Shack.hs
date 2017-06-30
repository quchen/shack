{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module System.Process.Shack where



import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Either
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text            as T
import           System.Exit
import           System.Process



data CmdOption = Cd FilePath

newtype Stdout a = Stdout a
newtype Stderr a = Stderr a
newtype Stdouterr a = Stdouterr a
newtype Exit = Exit ExitCode

data Str = Str String | Text T.Text | BS BS.ByteString | BSL BSL.ByteString | Unit
    deriving (Eq, Ord, Show)

data Pid = Pid0 | Pid ProcessHandle
instance Eq Pid where _ == _ = True -- Hack to make wrapping types derivable
instance Ord Pid where compare _ _ = EQ
instance Show Pid where show _ = "<PID>"

data Result
    = ResultStdout    Str
    | ResultStderr    Str
    | ResultStdouterr Str
    | ResultExit      ExitCode
    | ResultTime      Double
    | ResultLine      T.Text
    | ResultProcess   Pid
    deriving (Eq, Ord, Show)


class CmdString a where cmdString :: (Str, Str -> a)
instance CmdString [Char]         where cmdString = (Str mempty, \(Str  x) -> x)
instance CmdString T.Text         where cmdString = (Str mempty, \(Text x) -> x)
instance CmdString BS.ByteString  where cmdString = (BS mempty,  \(BS   x) -> x)
instance CmdString BSL.ByteString where cmdString = (BSL mempty, \(BSL  x) -> x)
instance CmdString ()             where cmdString = (Unit,       \_        -> ())

class CmdResult a where
    cmdResult :: ([Result], [Result] -> a)

instance CmdResult () where
    cmdResult = ([], \_ -> ())

instance CmdString a => CmdResult (Stdout a) where
    cmdResult
      = let (result, transformer) = cmdString
        in ([ResultStdout result], \[ResultStdout x] -> Stdout (transformer x))

instance CmdString a => CmdResult (Stderr a) where
    cmdResult
      = let (result, transformer) = cmdString
        in ([ResultStderr result], \[ResultStderr x] -> Stderr (transformer x))

instance CmdString a => CmdResult (Stdouterr a) where
    cmdResult
      = let (result, transformer) = cmdString
        in ([ResultStdouterr result], \[ResultStdouterr x] -> Stdouterr (transformer x))

instance (CmdResult a, CmdResult b) => CmdResult (a,b) where
    cmdResult
      = let (aResult, aTransformer) = cmdResult
            (bResult, bTransformer) = cmdResult
        in ( aResult <> bResult
           , \rs -> let (r1, r2) = splitAt (length aResult) rs
                    in (aTransformer r1, bTransformer r2))

-- |
-- prop> \xs ys -> splitByLength (xs :: [Int]) (ys :: [Int]) == splitAt (length xs) ys
-- prop> \xs ys -> splitByLength xs (xs ++ ys) == (xs :: [Int], ys)
splitByLength :: [x] -> [a] -> ([a], [a])
splitByLength []     ys     = ([], ys)
splitByLength _      []     = ([], [])
splitByLength (_:xs) (y:ys) = let (as, bs) = splitByLength xs ys
                              in (y:as, bs)

cmdResultWith :: CmdResult r => (r -> s) -> ([Result], [Result] -> s)
cmdResultWith f = let (results, transformer) = cmdResult
                  in (results, f . transformer)

instance CmdResult Exit where
    cmdResult = ([ResultExit undefined], \[ResultExit e] -> Exit e)

instance CmdResult ExitCode where
    cmdResult = ([ResultExit undefined], \[ResultExit e] -> e)

instance (CmdResult a, CmdResult b, CmdResult c) => CmdResult (a,b,c) where
    cmdResult = cmdResultWith (\(a,(b,c)) -> (a,b,c))

instance (CmdResult a, CmdResult b, CmdResult c, CmdResult d) => CmdResult (a,b,c,d) where
    cmdResult = cmdResultWith (\(a,b,(c,d)) -> (a,b,c,d))

instance (CmdResult a, CmdResult b, CmdResult c, CmdResult d, CmdResult e) => CmdResult (a,b,c,d,e) where
    cmdResult = cmdResultWith (\(a,b,c,(d,e)) -> (a,b,c,d,e))

instance (CmdResult a, CmdResult b, CmdResult c, CmdResult d, CmdResult e, CmdResult f) => CmdResult (a,b,c,d,e,f) where
    cmdResult = cmdResultWith (\(a,b,c,d,(e,f)) -> (a,b,c,d,e,f))

instance (CmdResult a, CmdResult b, CmdResult c, CmdResult d, CmdResult e, CmdResult f, CmdResult g) => CmdResult (a,b,c,d,e,f,g) where
    cmdResult = cmdResultWith (\(a,b,c,d,e,(f,g)) -> (a,b,c,d,e,f,g))

class CmdArgs a where
    cmdArgs :: [Either CmdOption T.Text] -> a

instance CmdResult r => CmdArgs (IO r) where
    cmdArgs args = case partitionEithers args of
        (opts, command@(_:_))
            -> let (results, transformer) = cmdResult
               in fmap transformer (runnyMcRunFace opts results (T.unwords command))
        (_, []) -> error "No executable or arguments given"

runnyMcRunFace :: [CmdOption] -> [Result] -> T.Text -> IO [Result]
runnyMcRunFace options expectedResults command = do
    let grabStdout = flip any expectedResults
            (\case ResultStdout{}    -> True
                   ResultStdouterr{} -> True
                   _other            -> False )
        grabStderr = flip any expectedResults
            (\case ResultStderr{}    -> True
                   ResultStdouterr{} -> True
                   _other            -> False )

    let optCd = listToMaybe [ x | Cd x <- options ]
    undefined
