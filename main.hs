import System.IO;

data DCuvant = Cuv {rep::String, frecventa::Int }
    deriving (Read, Show);


class CreezaDCuvant a where 
    creeazaDCuvant::String -> Int -> a;
    -- esteInLista::[a] -> a -> Bool; 

instance CreezaDCuvant DCuvant where
    creeazaDCuvant cuvant frec = ( Cuv {rep=cuvant, frecventa=frec })

citesteDinFisier::String -> IO()
citesteDinFisier numeFisier = do {
    hFis <- openFile numeFisier ReadMode;

    contents <- hGetContents hFis;
    
    singleWords <- return (words contents);

    frecventaPentruFiecareCuvant singleWords singleWords;

    hClose hFis;
}

frecventaPentruFiecareCuvant::[String] -> [String] -> IO()
frecventaPentruFiecareCuvant _ []  = return ();
frecventaPentruFiecareCuvant listaCompleta (x: xs) = do {
    numar <- return (wordFreq listaCompleta x 0);


    elem <- return (creeazaDCuvant x numar);
    
    frecventaPentruFiecareCuvant listaCompleta xs;
}

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
