import System.IO;
import System.Directory;
import System.Console.ANSI;
import Control.Exception;
import Data.List;

data DCuvant = Cuv {rep::String, frecventa::Int } deriving (Ord, Eq, Show);

class DCuvantFunctii a where
    -- pentru fiecare cuvant citit din fisier cauta de cate ori apare
    gasesteFrecventaPentruFiecareCuvant:: [String] -> [String] -> [a] -> [a]
    gasesteCelMaiFrecventCuvant:: [a] -> a -> a
    gasesteCelMaiPutinFrecventCuvant :: [a] -> a -> a
    cautaFrecventaCuvant:: String -> [a] -> IO Int
    afiasreDCuvinteCuPrinText:: [a] -> Int -> IO()
    -- returneaza al 3-lea element duntr-un tuple
    al3lea:: ([String], [String], [a]) -> [a]
    sortCrescSauDescresc:: [a] -> String -> [a]
    listaComparata:: [a] -> String -> Int -> [a]
    frecventaLiteraInceput:: [a] -> Char -> Int -> [a] -> (Int, [a])
    statisticiLitereInceput:: [Char] -> [a] -> [(Char, Int)] -> [(Char, Int)]


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

    -- afiseaza frecventa pentru fiecare DCuvant 
    afiasreDCuvinteCuPrinText [] _ = return ();
    afiasreDCuvinteCuPrinText (x: xs) randDeInceput = do {
        spatii <- return (repcar ' ' (18 - length (rep x)));
        printTextPeEcran randDeInceput 4 $ (rep x) ++ spatii  ++ " --- " ++ show (frecventa x) ++"\n";
        afiasreDCuvinteCuPrinText xs (randDeInceput + 1);
    }

    -- din tuple ul cu lista toate cuvinte , lista toate cuvinte fara dubluri, lista DCuvinte returneaza lista DCuvinte
    al3lea (_,_,a) = a;

    sortCrescSauDescresc listaFrecventa tipDeSortare = do {
        if tipDeSortare == "CRESC" then sortOn (\x -> negate (frecventa x)) listaFrecventa;
        else sortOn (\x -> (frecventa x)) listaFrecventa;
    }

    -- returneaza o lista cu elementele comparate mai mare, mai mic sau egal decat un numar
    listaComparata listaFrec tip numar | tip == "mare" = filter (\x -> (frecventa x) > numar ) listaFrec
                                       | tip == "mic" = filter (\x -> (frecventa x) < numar) listaFrec
                                       | otherwise = filter (\x -> (frecventa x) == numar) listaFrec

    frecventaLiteraInceput [] _ nrAparitii cuvinteCuLitera = (nrAparitii, cuvinteCuLitera);
    frecventaLiteraInceput (x:xs) litera nrAparitii cuvinte | head (rep x) == litera = frecventaLiteraInceput xs litera (nrAparitii + (frecventa x)) (cuvinte ++ [x])
                                                            | otherwise = frecventaLiteraInceput xs litera nrAparitii cuvinte
    
    statisticiLitereInceput [] _ arrOut = arrOut
    statisticiLitereInceput (x:xs) cuvinte arrOut = do {

        numar <- return (frecventaLiteraInceput cuvinte x 0 []);
        statisticiLitereInceput xs cuvinte (arrOut ++ [(x, (fst numar))]);
    }

-- ia un string mare si intoarce o lista de cuvinte, impartiera de face pe caracterele spatiu, punct, virgula, linie noua ! @ ' "
listareCuvinte:: String -> String -> [String] -> [[String]]
listareCuvinte [] cuv  toateCuv = do {
    return (toateCuv ++ [cuv]);
}
listareCuvinte (x: xs) cuvantCurent toateCuvintele = do {
    if( x == ' ' || x == '.' || x == ',' || x == '\n' || x == '!' || x == '?' || x == '\'' || x == '\"') then do {
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
-- returneaza un tuple cu lista cu toate cuvintele si un o lista cu toate cuvintele dar fara sa se repete
citesteDinFisier:: String -> IO([String], [String])
citesteDinFisier  numeFis = do {

    contents <- readFile numeFis;

    singleWords <- return (head (listareCuvinte contents "" []));

    singleWordsFaraSpatii <- return (filter (not . null) singleWords);

    return (singleWordsFaraSpatii, (stergeDuplicate  singleWordsFaraSpatii))
}
-- sterge elementele duplicate dintr-o lista
-- `elem` returneaza adevarat daca lista(al 2-lea argument) contine elementul (1 argument)
stergeDuplicate :: [String] -> [String]
stergeDuplicate = dacaAfostVazut []
    where dacaAfostVazut vazut [] = vazut
          dacaAfostVazut vazut (x:xs)
              | x `elem` vazut = dacaAfostVazut vazut xs
              | otherwise = dacaAfostVazut (vazut ++ [x]) xs


sterge :: [String] -> [String] -> IO [String]
sterge [] lista = return lista
sterge (x:xs) lista = do {
    if x `elem` lista then sterge xs lista
    else sterge xs (lista ++ [x])
}

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

-- ia un caracter a si un numar b si returneaza un string cu a repetat de b ori 
repcar::Char -> Int -> String;
repcar car 0 = [];
repcar car nr = car:(repcar car (nr - 1));

-- citeste alegerea utilizatorului din meniu, daca nu este un numar intreg arunca o eroare
citesteAlegereUser:: String -> Int -> Int -> IO Int
citesteAlegereUser cuvant rand coloana = do {

    printTextPeEcran rand coloana cuvant;
    nr <- getLine;
    recit <- try (evaluate (read nr::Int))::IO  (Either SomeException Int);
    case recit of
    {
        Left exc -> setCursorPosition rand coloana>>clearFromCursorToLineEnd>>setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]>>(printTextPeEcran (rand+1) coloana $ "Trebuie sa introduceti un numar de la 0 - 9. Eroare: " ++ show exc)>>citesteAlegereUser cuvant rand coloana;

        Right value -> clearScreen>>return value;
    }
}

-- afisarea meniului
afiseazaMeniu:: String -> IO Int
afiseazaMeniu numeFis = do {
    printTextPeEcran 0 4 "Fisierul curent: ";
    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ];
    printTextPeEcran 0 21 numeFis;

    printTextPeEcran 1 4 "1. Schimbati fisierul. \n";
    printTextPeEcran 2 4 "2. Afisare cel mai frecvent cuvant.\n";
    printTextPeEcran 3 4 "3. Afisare cel mai putin frecvent cuvant.\n";
    printTextPeEcran 4 4 "4. Gaseste frecventa unui cuvant.\n";
    printTextPeEcran 5 4 "5. Afiseaza toate cuvintele.\n";
    printTextPeEcran 6 4 "6. Afisati cuvintele dupa frecventa.\n";
    printTextPeEcran 7 4 "7. Adaugati un rand nou la finalul fisierului.\n";
    printTextPeEcran 8 4 "8. Afisare cuvinte in functie de litera de inceput.\n";
    printTextPeEcran 9 4 "9. Afisare statistici litere inceput \n";

    printTextPeEcran 11 4 "0. Tastati 0 pentru iesire.\n";

    citesteAlegereUser "Alegere: " 13 4;
}


-- primeste numeleFisierul si returneaza toate listele necesare in rularea programului ([toateCuvintele], [toateCuvinteleFaraDubluri], [listaCuFrecvente])
returnareToateListele::String -> IO ([String], [String], [DCuvant])
returnareToateListele numeFisier= do {
    liste <- (citesteDinFisier numeFisier);
    listaCompleta <- (return (fst liste));
    listaFaraDubluri <- return (snd liste);
    listaFrecvente <- return (gasesteFrecventaPentruFiecareCuvant listaCompleta listaFaraDubluri ([]:: [DCuvant]));
    return (listaCompleta, listaFaraDubluri, listaFrecvente);
}

-- afiseaza cel mai frecvent cuvant
afiseazaCelMaiFrecventCuvant::String -> IO()
afiseazaCelMaiFrecventCuvant numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    celMaiFrecventCuv <- return (gasesteCelMaiFrecventCuvant listaFrecvente (Cuv "" 0));
    clearScreen;
    printTextPeEcran 1 4 $ "Cel mai frecvent cuvant este: \"" ++ (rep celMaiFrecventCuv) ++ "\", numar aparitii: " ++ show (frecventa celMaiFrecventCuv);
    printTextPeEcran 2 4 "Apasati enter pentru meniu";

    getLine;

    clearScreen;
    runMeniu numeFisier;

}
-- afiseaza cel mai putin frecvent cuvant 
afiseazaCelMaiPutinFrecventCuvant::String -> IO()
afiseazaCelMaiPutinFrecventCuvant numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecvente <- return (al3lea liste);

    celMaiPutinFrecvent <- return (gasesteCelMaiPutinFrecventCuvant listaFrecvente (Cuv "test" 10000));
    clearScreen ;
    printTextPeEcran 1 4 $ "Cel mai putin frecvent cuvant este: \"" ++ (rep celMaiPutinFrecvent) ++ "\", numar de aparitii: " ++ show (frecventa celMaiPutinFrecvent);
    printTextPeEcran 2 4 "Apasati enter pentru meniu";

    getLine;

    clearScreen;
    runMeniu numeFisier;
}

-- afiseaza frecvena unui cuvant dat de user 
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

    printTextPeEcran 5 4 "Apasati enter pentru meniu";

    getLine;

    clearScreen;
    runMeniu numeFisier;
}

-- afisare formatata pentru fiecare litere din alfabet cu frecventa
afisareListaStatisticiLitere::[(Char, Int)] -> Int -> IO()
afisareListaStatisticiLitere [] _ = return ()
afisareListaStatisticiLitere (x:xs) randInceput = do {
    spatiiLitere <- return $ repcar ' ' 2;
    spatiiNumere <- return $ repcar ' '  (6 - (length (show (snd x))));
    printTextPeEcran randInceput 4 $ [(fst x)] ++ spatiiLitere ++ "---" ++ spatiiNumere ++ (show (snd x));
    afisareListaStatisticiLitere xs (randInceput +1);
}

-- afisare pentru litere alfabet cu frecventa
afisareStatisticiLitereInceput::String -> IO ()
afisareStatisticiLitereInceput numeFisier = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecventa <- return (al3lea liste); 

    listaLitereSiFrecventa <- return (statisticiLitereInceput ['a'..'z'] listaFrecventa []);
    
    clearScreen ;

    afisareListaStatisticiLitere listaLitereSiFrecventa 2;

    pozitieCursor <- (getCursorPosition) ;
    pozitie <- return (fmap fst pozitieCursor);

    case pozitie of {
        Just pozitie -> printTextPeEcran (pozitie + 2) 4 "Apasati enter pentru meniu";
        Nothing -> printTextPeEcran 2 4 "Apasati enter pentru meniu";
    };

    getLine ;

    clearScreen ;
    runMeniu numeFisier;
}

-- scrie la finalul fisieruli o propozitie sau un cuvant
scrieLaFinalulFisierului:: String -> IO()
scrieLaFinalulFisierului numeFisier = do {
    hFisier <- openFile numeFisier AppendMode;
    clearScreen ;
    printTextPeEcran 2 4 "Adaugati text la finalul fisierului:\n";
    setSGR ([ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green ]);
    setCursorPosition 3 4;
    linieNoua <- getLine;
    hPutStrLn hFisier linieNoua;
    hClose hFisier;

    pozitieCursor <- (getCursorPosition) ;
    pozitie <- return (fmap fst pozitieCursor);

    setSGR ([Reset ]);
    case pozitie of {
        Just pozitie -> printTextPeEcran (pozitie + 1) 4 "Apasati enter pentru meniu";
        Nothing -> printTextPeEcran 2 4 "Apasati enter pentru meniu";
    };

    getLine;

    clearScreen ;
    runMeniu numeFisier;

}

-- afiseaza frecventa si cuvintele care incep cu o litera de la user
afisareFrecventaLiteraDeInceput::String -> IO()
afisareFrecventaLiteraDeInceput numeFisier = do {
    liste <- returnareToateListele(numeFisier);
    listaFrecventa <- return (al3lea liste);

    clearScreen ;
    printTextPeEcran 2 4 "Scrieti o litera: ";
    litera <- getChar ;
    aparTuple <- return (frecventaLiteraInceput listaFrecventa litera 0 []);
    aparitii <-  return (fst aparTuple);
    cuvinte <- return (snd aparTuple);

    afiasreDCuvinteCuPrinText cuvinte 4;

    pozitieCursor <- getCursorPosition ;

    rand <- return (fmap fst pozitieCursor);

    case rand of {
        Just rand -> setSGR ([SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow ])>>(printTextPeEcran (rand+1) 4 $ (show aparitii) ++ " cuvinte incep cu litera " ++ [litera] ++ " .")>>printTextPeEcran (rand +3) 4 "Apasati enter pentru meniu";
        Nothing -> (printTextPeEcran 4 4 $ "Nu exista cuvinte care incep cu litera: " ++ [litera])>>printTextPeEcran 5 4 "Apasati enter pentru meniu"
    };

    getLine ;
    getLine ;
    clearScreen ;
    runMeniu numeFisier;
}

-- afiseaza daca nu au fost gasit fisiere pentru functia de mai jos
afisareFinalPentruCuvinteDupaFrecvent::String -> Int -> IO()
afisareFinalPentruCuvinteDupaFrecvent numeFis l = do {
    case l of 
        {
            0 -> printTextPeEcran (l) 4 "Nu au fost gasite cuvnite."; 
            _ -> printTextPeEcran 0 0 "";
        };

        printTextPeEcran (l+2) 4 "Apasati enter pentru meniu";
        getLine;
        clearScreen ;
        runMeniu numeFis;
}


-- face afisare de cuvinte in functie de frecventa, daca este mai mica mare sau egala fata de un numar introdus de user
afisareCuvinteDupaFrecventa:: String -> Int -> IO()
afisareCuvinteDupaFrecventa numeFisier numarIntrodus = do {
    liste <- (returnareToateListele numeFisier);
    listaFrecventa <- return (al3lea liste);
    clearScreen ;

    if numarIntrodus == -1 then do {
        numar <- (citesteAlegereUser "Numar de comparat: " 2 4);
        afisareCuvinteDupaFrecventa numeFisier numar;
    } else do {
        clearScreen ;
        printTextPeEcran 1 4 $ "Numar de comaparat: " ++ (show numarIntrodus);
        printTextPeEcran 2 4 "1. Afisati cuvintele cu frecventa mai mari.";
        printTextPeEcran 3 4 "2. Afisati cuvintele cu frecventa mai mici";
        printTextPeEcran 4 4 "3. Afisati cuvintele cu frecventa egala." ;
        printTextPeEcran 5 4 "0. Intoarcere la meniu" ;

        numar <- (citesteAlegereUser "Alegere: " 7 4);
        l <- return (2);
        case numar of {
            1 -> do {
                clearScreen; 
                lista <- return $ listaComparata listaFrecventa "mare" numarIntrodus;
                l <- return (length lista);
                afiasreDCuvinteCuPrinText lista 1;
                afisareFinalPentruCuvinteDupaFrecvent numeFisier l;
            };
            2 -> do {
                clearScreen; 
                lista <- return (listaComparata listaFrecventa "mic" numarIntrodus);
                l <- return (length lista);
                afiasreDCuvinteCuPrinText lista 1;
                afisareFinalPentruCuvinteDupaFrecvent numeFisier l;
            };
            3 -> do {
                clearScreen; 
                lista <- return (listaComparata listaFrecventa "egal" numarIntrodus);
                l <- return (length lista);
                afiasreDCuvinteCuPrinText lista 1;
                afisareFinalPentruCuvinteDupaFrecvent numeFisier l;
            };
            0 -> runMeniu numeFisier;
            _ -> afisareCuvinteDupaFrecventa numeFisier numarIntrodus;
        };
    }

    
}

-- printeaza lista de dcuvinte crescator sau descrescator
printListaFrecventa:: [DCuvant] -> Int -> IO()
printListaFrecventa listaCuv tipAfisare 
                    | tipAfisare == 1 = afiasreDCuvinteCuPrinText (sortCrescSauDescresc listaCuv "CRESC") 1
                    | tipAfisare == 2 = afiasreDCuvinteCuPrinText (sortCrescSauDescresc listaCuv "DESCRESC") 1
                    | otherwise = afiasreDCuvinteCuPrinText listaCuv 1

-- afiseaza frecventa pentru toate cuvintele 
afisareToateCuvintele:: String -> Int -> IO()
afisareToateCuvintele numeFis tipAfisare = do {
    liste <- (returnareToateListele numeFis);
    listaFrecventa <- return (al3lea liste);

    marimeListaFrecventa <- return (length listaFrecventa);

    clearScreen;

    printListaFrecventa listaFrecventa tipAfisare;
    
    putStrLn "";
    putStrLn "Apasati enter pentru meniu";
    putStrLn "Apasati 1 pentru sortare crescatoare.";
    putStrLn "Apasati 2 pentru sortare descrescatoare.";

    ordonare <- getLine;

    if ordonare == "1" then afisareToateCuvintele numeFis 1
    else if ordonare == "2" then afisareToateCuvintele numeFis 2
    else clearScreen >> runMeniu numeFis
} 

-- functie pentru inceperea programului din meniu (fara input si verificare a fisierului din care se citeste)
runMeniu::String -> IO()
runMeniu numeFisier = do {
    alegere <- afiseazaMeniu numeFisier;
    case alegere of
    {
        1 -> runProgram;
        2 -> afiseazaCelMaiFrecventCuvant numeFisier;
        3 -> afiseazaCelMaiPutinFrecventCuvant numeFisier;
        4 -> afiseazaFrecventaCuvantDeLaUser numeFisier;
        5 -> afisareToateCuvintele numeFisier 0;
        6 -> afisareCuvinteDupaFrecventa numeFisier (-1);
        7 -> scrieLaFinalulFisierului numeFisier;
        8 -> afisareFrecventaLiteraDeInceput numeFisier;
        9 -> afisareStatisticiLitereInceput numeFisier;
        0 -> return ();
        _ -> setCursorPosition 13 4>>clearFromCursorToLineEnd>>setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]>>printTextPeEcran 14 4 "Alegerea nu exista.">>runMeniu numeFisier;
    }
}

-- intrare in program
runProgram:: IO()
runProgram = do {
    clearScreen;
    numeFisier <- citesteNumeFisierSiVerificaDacaExista;
    if numeFisier == "0" then return ()
    else runMeniu numeFisier
   
}

main = runProgram