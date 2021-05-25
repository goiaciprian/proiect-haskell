import System.IO;

data DCuvant = Cuv {rep::String, frecventa::Int }
    deriving (Read, Show);

-- Citeste dintr-un fisier continutul si adauga fiecare cuvant intr-o lista
citesteDinFisier::String -> IO()
citesteDinFisier numeFisier = do {
    hFis <- openFile numeFisier ReadMode;

    contents <- hGetContents hFis;
    
    singleWords <- return (words contents);

    print (stergeDuplicate singleWords);

    hClose hFis;
}

-- `elem` returneaza adevarat daca lista(al 2-lea argument) contine elementul (1 argument)
stergeDuplicate :: [String] -> [String]
stergeDuplicate = rdHelper []
    where rdHelper vazut [] = vazut
          rdHelper vazut (x:xs)
              | x `elem` vazut = rdHelper vazut xs
              | otherwise = rdHelper (vazut ++ [x]) xs

-- returneaza de cate ori apare un cuvant in lista data
wordFreq :: [String] -> String -> Int -> Int
wordFreq [] _ nr = nr
wordFreq (x:xs) cuv nr = do {
    if cuv == x 
        then do wordFreq xs cuv (nr+1)
    else wordFreq xs cuv nr
}

main = do {
    -- stringCuv <- return "mama mama ce ce functie am am gasit";
    
    -- listaCuvinte <- return (words stringCuv);

    -- print (wordFreq listaCuvinte "mama" 0);

    -- print listaCuvinte;

    citesteDinFisier "input.in";
}
