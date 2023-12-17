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

digits' :: [Char] -> [Char]
digits' [] = []
digits' ('o':'n':'e':cs) = '1' : digits' ('e':cs)
digits' ('t':'w':'o':cs) = '2' : digits' ('o':cs)
digits' ('t':'h':'r':'e':'e':cs) = '3' : digits' ('e':cs)
digits' ('f':'o':'u':'r':cs) = '4' : digits' cs
digits' ('f':'i':'v':'e':cs) = '5' : digits' ('e':cs)
digits' ('s':'i':'x':cs) = '6' : digits' cs
digits' ('s':'e':'v':'e':'n':cs) = '7' : digits' ('n':cs)
digits' ('e':'i':'g':'h':'t':cs) = '8' : digits' ('t':cs)
digits' ('n':'i':'n':'e':cs) = '9' : digits' ('e':cs)
digits' (c:cs)
  | c `elem` ['0'..'9'] = c : digits' cs
  | otherwise = digits' cs

solve' :: [String] -> Int
solve' = sum . map (calibration . digits')

main = do
  input <- readLines "01/example"
  print $ solve input
  input <- readLines "01/puzzle"
  print $ solve input
  input <- readLines "01/example2"
  print $ solve' input
  input <- readLines "01/puzzle"
  print $ solve' input
