import List
import System
import Text.Printf


main :: IO ()
main = main1


main0 :: IO ()
main0 =
     do fns <- getArgs
        mapM_ wc0 fns

wc0 :: FilePath -> IO ()
wc0 fn =
     do cts <- readFile fn
        putStr $ printf "%4d %4d %4d %s\n"
                        (length(lines cts)) (length(words cts)) (length cts) fn


main1 :: IO ()
main1 =
     do fns <- getArgs
        cs  <- mapM wc3 fns
        let (lcs,wcs,ccs) = unzip3 cs
        case cs of
          _:_:_ -> putStr $ printf "%4d %4d %4d total\n"
                                                (sum lcs) (sum wcs) (sum ccs)
          _     -> return ()


wc1 :: FilePath -> IO (Int,Int,Int)
wc1 fn =
     do cts <- readFile fn
        let (lc,wc,cc) = count2 cts
        putStr $ printf "%4d %4d %4d %s\n" lc wc cc fn
        return (lc,wc,cc)

count1 :: String -> (Int,Int,Int)
count1 cts = (length(lines cts),length(words cts),length cts)

count2 :: String -> (Int,Int,Int)
count2 cts = foldl cl (0,0,0) $ lines' cts
      where
        cl (lc,wc,cc) (ln,nl) = (lc',wc',cc')
              where
                lc' = lc + 1
                wc' = wc + length(words ln)
                cc' = cc + length ln + (if nl then 1 else 0)


wc3 :: FilePath -> IO (Int,Int,Int)
wc3 fn =
     do cts <- readFile fn
        let C lns wds chs = count3 cts
        putStr $ printf "%4d %4d %4d %s\n" lns wds chs fn
        return (lns,wds,chs)

data Counts = C !Int !Int !Int                                  deriving (Show)

count3 :: String -> Counts
count3 cts = foldl cl (C 0 0 0) $ lines' cts
      where
        cl (C lc wc cc) (ln,nl) = C lc' wc' cc'
              where
                lc' = lc + 1
                wc' = wc + length(words ln)
                cc' = cc + length ln + (if nl then 1 else 0)


lines' :: String -> [(String,Bool)]
lines' ""  = []
lines' str = case break (=='\n') str of
               (ln,[] ) -> [(ln,False)]
               (ln,_:t) ->  (ln,True ) : lines' t
