open Printf

let fibonacci nbTermes =
        let rec fibonacci un j compteur =
                if compteur < nbTermes then
                        let compteur' = compteur + 1 in
                        printf "%d " j;
                        let i' = un + j in
                        fibonacci j i' compteur'
        in
        fibonacci 0 1 0;;

fibonacci 40;;
printf "\n";;
