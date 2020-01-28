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
        let checkN n =
                match n with
                | 0 -> false
                | 1 -> false
                | 2 -> true
                | _ -> if n mod 2 = 0 then false else isPrime 3
        in
        checkN n;;


let boucle n =
        let rec boucle nb i =
                let rec i' =
                        if isPrime nb
                                then i + 1
                                else i in
                if i' = i + 1 then printf "%d : %d is prime \n" i nb;
                if nb < n then boucle (nb + 1) i'
        in
        boucle 0 1;;

boucle 1000;;
