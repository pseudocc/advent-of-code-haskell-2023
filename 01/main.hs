readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

digits :: [Char] -> [Char]
digits = filter (`elem` ['0'..'9'])

calibration :: [Char] -> Int
calibration cs = read [x, y] :: Int
  where
    x = head cs
    y = last cs

solve :: [String] -> Int
solve = sum . map (calibration . digits)

main = do
  input <- readLines "01/example"
  print $ solve input
  input <- readLines "01/puzzle"
  print $ solve input
