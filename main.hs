import System.IO;
import System.Directory;
import System.Console.ANSI;
import Control.Exception;


data DCuvant = Cuv {rep::String, frecventa::Int }
    deriving (Read, Show);

class DCuvantFunctii a where
    -- pentru fiecare cuvant citit din fisier cauta de cate ori apare
    gasesteFrecventaPentruFiecareCuvant:: [String] -> [String] -> [a] -> [a]
    gasesteCelMaiFrecventCuvant:: [a] -> a -> a
    gasesteCelMaiPutinFrecventCuvant :: [a] -> a -> a
    cautaFrecventaCuvant:: String -> [a] -> IO Int
    afiasreDCuvinteCuPrinText:: [a] -> Int -> IO()

instance DCuvantFunctii DCuvant where
    gasesteFrecventaPentruFiecareCuvant _ [] cuvinte = cuvinte;
    gasesteFrecventaPentruFiecareCuvant toateCuvintele (x: xs) arrCuvinte = do {
        numarDeAparitii <- return (wordFreq toateCuvintele x 0);
        cuvantNou <- return (Cuv x numarDeAparitii);
        arrCuvinteNoi <- return (arrCuvinte ++ [cuvantNou]);
        gasesteFrecventaPentruFiecareCuvant toateCuvintele xs arrCuvinteNoi;
    }

    gasesteCelMaiFrecventCuvant [] cuvant = cuvant;
    gasesteCelMaiFrecventCuvant (x: xs) cuvant = do {
        if (frecventa x) > (frecventa cuvant) then gasesteCelMaiFrecventCuvant xs x
        else gasesteCelMaiFrecventCuvant xs cuvant
    }

    gasesteCelMaiPutinFrecventCuvant [] cuvant = cuvant;
    gasesteCelMaiPutinFrecventCuvant (x: xs) cuvant = do {
        if (frecventa x) < (frecventa cuvant) then gasesteCelMaiPutinFrecventCuvant xs x
        else gasesteCelMaiPutinFrecventCuvant xs cuvant
    }


    cautaFrecventaCuvant repCuvant toateCuvintele =  do {
        cuvinteGasite <- return (filter (\x -> rep x == repCuvant) toateCuvintele);

        if null cuvinteGasite then do {
            return (-1);
        } else do {

            firstElem <- return (head cuvinteGasite);
            return (frecventa firstElem);
        }
    }

    afiasreDCuvinteCuPrinText [] _ = return ();
    afiasreDCuvinteCuPrinText (x: xs) randDeInceput = do {
        spatii <- return (repcar ' ' (10 - length (rep x)));
        printTextPeEcran randDeInceput 4 $ (rep x) ++ spatii  ++ " --- " ++ show (frecventa x) ++ "\n";
        afiasreDCuvinteCuPrinText xs (randDeInceput + 1);
    }

listareCuvinte:: String -> String -> [String] -> [[String]]
listareCuvinte [] cuv  toateCuv = do {
    return (toateCuv ++ [cuv]);
}
listareCuvinte (x: xs) cuvantCurent toateCuvintele = do {
    if( x == ' ' || x == '.' || x == ',' || x == '\n') then do {
        if(cuvantCurent == "") then listareCuvinte xs "" toateCuvintele
        else do {
            toateCuvUpdate <- return (toateCuvintele ++ [cuvantCurent]);
            listareCuvinte xs "" toateCuvUpdate;
        }
    } else do {
        cuvantCurentNout <- return (cuvantCurent ++ [x]);
        listareCuvinte xs cuvantCurentNout toateCuvintele;
    }
}

-- Citeste dintr-un fisier continutul si adauga fiecare cuvant intr-o lista
citesteDinFisier:: String -> IO([String], [String])
citesteDinFisier  numeFis = do {

    contents <- readFile numeFis;

    singleWords <- return (head (listareCuvinte contents "" []));

    return (singleWords, (stergeDuplicate  singleWords))
}
-- sterge elementele duplicate dintr-o lista
-- `elem` returneaza adevarat daca lista(al 2-lea argument) contine elementul (1 argument)
stergeDuplicate :: [String] -> [String]
stergeDuplicate = dacaAfostVazut []
    where dacaAfostVazut vazut [] = vazut
          dacaAfostVazut vazut (x:xs)
              | x `elem` vazut = dacaAfostVazut vazut xs
              | otherwise = dacaAfostVazut (vazut ++ [x]) xs

-- returneaza de cate ori apare un cuvant in lista data
wordFreq :: [String] -> String -> Int -> Int
wordFreq [] _ nr = nr
wordFreq (x:xs) cuv nr = do {
    if cuv == x
        then do wordFreq xs cuv (nr+1)
    else wordFreq xs cuv nr
}

-- citeste un string de la tastatura si verifica daca este sau nu un path pentru un fisier
citesteNumeFisierSiVerificaDacaExista:: IO String
citesteNumeFisierSiVerificaDacaExista = do {

    printTextPeEcran 2 4 "Nume fisier:";
    numeFisier <- getLine;

    if numeFisier == "0" then return numeFisier;
    else do {
        existaFisierul <- doesFileExist numeFisier;

            if(existaFisierul) then do clearScreen>>return numeFisier;
            else do {
                clearScreen;
                setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red];
                printTextPeEcran 3 4 "Fisierul nu exista.Tastati 0 pentru iesire.";
                citesteNumeFisierSiVerificaDacaExista;

        }
    }

}

-- printeaza pe ecran cu ANSI 
printTextPeEcran:: Int -> Int -> String -> IO ()
printTextPeEcran rand coloana text = do {
    setCursorPosition rand coloana;
    putStr text;
    hFlush stdout;
    setSGR [Reset];
}

repcar::Char -> Int -> String;
repcar car 0 = [];
repcar car nr = car:(repcar car (nr - 1));


citesteAlegereUser:: IO Int
citesteAlegereUser = do {

    printTextPeEcran 9 4 "Alegere: ";
    nr <- getLine;
    recit <- try (evaluate (read nr::Int))::IO  (Either SomeException Int);
    case recit of
    {
        Left exc -> setCursorPosition 9 4>>clearFromCursorToLineEnd>>setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]>>(printTextPeEcran 10 4 $ "Eroare: " ++ show exc)>>citesteAlegereUser;

        Right value -> clearScreen>>return value;
    }
}

afiseazaMeniu::IO Int
afiseazaMeniu = do {
    printTextPeEcran 1 4 "1. Schimbati fisierul. \n";
    printTextPeEcran 2 4 "2. Afisare cel mai frecvent cuvant.\n";
    printTextPeEcran 3 4 "3. Afisare cel mai putin frecvent cuvant.\n";
    printTextPeEcran 4 4 "4. Gaseste frecventa unui cuvant.\n";
    printTextPeEcran 5 4 "5. Afiseaza toate cuvintele.\n";

    printTextPeEcran 7 4 "0. Tastati 0 pentru iesire.\n";

    citesteAlegereUser;
}

al3lea:: ([String], [String], [DCuvant]) -> [DCuvant]
al3lea (_,_,a) = a;

returnareToateListele::String -> IO ([String], [String], [DCuvant])
returnareToateListele numeFisier= do {
    liste <- (citesteDinFisier numeFisier);
    listaCompleta <- (return (fst liste));
    listaFaraDubluri <- return (snd liste);
    listaFrecvente <- return (gasesteFrecventaPentruFiecareCuvant listaCompleta listaFaraDubluri ([]:: [DCuvant]));
    return (listaCompleta, listaFaraDubluri, listaFrecvente);
}

afiseazaCelMaiFrecventCuvant::String -> IO()
afiseazaCelMaiFrecventCuvant numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    celMaiFrecventCuv <- return (gasesteCelMaiFrecventCuvant listaFrecvente (Cuv "" 0));
    clearScreen;
    printTextPeEcran 1 4 $ "Cel mai frecvent cuvant este: \"" ++ (rep celMaiFrecventCuv) ++ "\", numar aparitii: " ++ show (frecventa celMaiFrecventCuv);
    printTextPeEcran 2 4 "Apasati orice tasta pentru a va intoarce la meniu.";

    getLine;

    clearScreen;
    runMeniu numeFisier;

}

afiseazaCelMaiPutinFrecventCuvant::String -> IO()
afiseazaCelMaiPutinFrecventCuvant numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    celMaiPutinFrecvent <- return (gasesteCelMaiPutinFrecventCuvant listaFrecvente (Cuv "test" 10000));
    clearScreen ;
    printTextPeEcran 1 4 $ "Cel mai putin frecvent cuvant este: \"" ++ (rep celMaiPutinFrecvent) ++ "\", numar de aparitii: " ++ show (frecventa celMaiPutinFrecvent);
    printTextPeEcran 2 4 "Apasati orice tasta pentru a va intoarce la meniu.";

    getLine;

    clearScreen;
    runMeniu numeFisier;
}

afiseazaFrecventaCuvantDeLaUser:: String -> IO ()
afiseazaFrecventaCuvantDeLaUser numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    clearScreen ;

    printTextPeEcran 2 4 "Cuvant: ";
    cuvant <- getLine;

    frecventaCuvant <- (cautaFrecventaCuvant cuvant listaFrecvente);

    if frecventaCuvant == -1 then setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]>>printTextPeEcran 4 4 "Cuvantul nu a fost gasit.";
    else printTextPeEcran 4 4 $ "Pentru cuvantul: \"" ++ cuvant ++ "\" frecventa este: " ++ show frecventaCuvant ++ ".";

    printTextPeEcran 5 4 "Apasati orice tasta pentru a va intoarce la meniu.";

    getLine;

    clearScreen;
    runMeniu numeFisier;
}

afiseazaToateCuvintele:: String -> IO()
afiseazaToateCuvintele numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    clearScreen ;

    afiasreDCuvinteCuPrinText listaFrecvente 1;

    printTextPeEcran ((length listaFrecvente) + 2) 4 "Apasati orice tasta pentru a va intoarce la meniu.";

    getLine;

    clearScreen;
    runMeniu numeFisier;
}

runMeniu::String -> IO()
runMeniu numeFisier = do {
    alegere <- afiseazaMeniu;
    case alegere of
    {
        1 -> runProgram;
        2 -> afiseazaCelMaiFrecventCuvant numeFisier;
        3 -> afiseazaCelMaiPutinFrecventCuvant numeFisier;
        4 -> afiseazaFrecventaCuvantDeLaUser numeFisier;
        5 -> afiseazaToateCuvintele numeFisier;
        0 -> return ();
        _ -> setCursorPosition 9 4>>clearFromCursorToLineEnd>>setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]>>printTextPeEcran 10 4 "Alegerea nu exista.">>runMeniu numeFisier;
    }
}

runProgram:: IO()
runProgram = do {
    clearScreen;
    numeFisier <- citesteNumeFisierSiVerificaDacaExista;
    if numeFisier == "0" then return ()
    else runMeniu numeFisier
   
}

main = runProgram