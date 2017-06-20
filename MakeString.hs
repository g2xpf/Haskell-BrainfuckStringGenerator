{-# LANGUAGE CPP #-}
#define FF if True then "\n" else []
import Data.Char

doubling :: Int -> Int
doubling n = n * n

-- nを引数として, n = xy を満たす正の整数x, yに対して x + y を最小にするの組(x, y)を返す関数
makeShortMult :: Int -> (Int, Int)
makeShortMult n 
  | n >= 0  = let root = floor . sqrt . fromIntegral $ n
              in (root, n - (doubling root)) 
  | otherwise = let root = floor . sqrt . fromIntegral $ (-n)
                in ((-root), (-n) - (doubling root))

makeCodeFromPare :: (Int, Int) -> String
makeCodeFromPare (root, rem) = let sign = if root >= 0 then "+" else "-"
                                   root' = abs root
                               in (if root == 0 then "." else (concat $ replicate rem sign) ++ '>' : (concat $ replicate root' "+") ++ "[<" ++ (concat $ replicate root' sign) ++ ">-]<.")

inputStringToNum :: String -> [Int]
inputStringToNum xs = map ord (xs++FF)

makeDiffList' :: [Int] -> [Int]
makeDiffList' [] = []
makeDiffList' [x] = [x]
makeDiffList' [x,y] = [y - x]
makeDiffList' (x:y:xs) = y - x : makeDiffList' (y:xs)

makeDiffList :: [Int] -> [Int]
makeDiffList [] = []
makeDiffList [x] = [x]
makeDiffList (x:xs) = x : makeDiffList' (x:xs)


main :: IO ()
main = do
  str <- getLine
  let code = concat . map (makeCodeFromPare . makeShortMult) . makeDiffList . inputStringToNum $ str
  putStrLn code

