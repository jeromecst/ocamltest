open Printf

let isPrime n =
        let rec isPrime i =
                if float_of_int i <= sqrt (float_of_int n)
                        then
                        if n mod i = 0
                                then false
                                else isPrime (i + 2)
                        else true
        in
        match n with
        | 0 -> false
        | 1 -> false
        | 2 -> true
        | _ -> if n mod 2 = 0 then false else isPrime 3
;;


let nextPrime n =
        let rec nextPrime n =
                if isPrime n then n else nextPrime (n + 2)
        in
        if n = 2 then nextPrime (n + 1) else nextPrime (n + 2)
;;


let premier_diviseurs n =
        let rec premier_diviseurs i =
                if n mod i = 0 then i else premier_diviseurs (nextPrime i)
        in
        premier_diviseurs 2
;;

let decomposition n =
        let rec decomposition n =
                let diviseur = premier_diviseurs n in
                Printf.printf "x %d " diviseur;
                let n' = n / diviseur in
                if n' > 1 then decomposition n'
        in
        Printf.printf "%d = 1 " n;
        decomposition n;
        Printf.printf "\n"
;;

let n = int_of_string Sys.argv.(1);;
decomposition n;;

let boucle max =
        let rec boucle i =
                decomposition i;
                if i < max then boucle (i+1)
        in
        boucle 4;;

(* boucle 2023459;; *)
