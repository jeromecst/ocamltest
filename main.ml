open Printf

let somme n =
    let rec somme i acc =
        match i with
        | 0 -> acc
        | _ -> let i' = i - 1 in let acc' = acc + i in somme i' acc'
        in
    somme n 0;;


let fact_naif n =
    let rec fact_naif n =
        match n with
        | 0 -> 1
        | _ -> n * fact_naif(n - 1)
    in
    if n < 0 then failwith "argument négatif" else fact_naif n;;

let fact n =
        let rec fact i acc =
                match i with
                | 0 -> acc
                | _ -> let i' = i - 1 in let acc' = acc * i in fact i' acc'
        in
        if n < 0 then failwith "argument négatif" else fact n 1;;


let somme_naif n =
    let rec somme_naif n =
        match n with
        | 0 -> n
        | _ -> n + somme_naif (n - 1)
    in
    somme_naif n;;

let n = 1000000;;
let s' = somme n;;
let f = fact_naif 20;;
let f' = fact 20;;
printf "somme = %d fac_natif = %d fac = %d \n" s' f f';;
