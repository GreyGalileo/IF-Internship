(*
The purpose of this module is to use Berlekamp's algorithm 
to find an irreducible polynomial over F2[x]
*)
type polynomial = int list

exception WrongDimension of int

let rec eliminate_leading_0s (p:int list) = 
  match p with
  | 0 :: res -> eliminate_leading_0s res
  | _ -> p;;

let rec remainder_euclidean_division_polynomials: polynomial -> polynomial -> polynomial
  = fun p q ->  (*remainder of the euclidean division of p by q*)
  let p_red = eliminate_leading_0s p and q_red = eliminate_leading_0s q in
  let p_len = List.length p_red and q_len = List.length q_red in
  if p_len < q_len || p_len = 0 then p_red
  else 
    let q_shifted = q_red @ List.init (p_len - q_len) (fun _ -> 0) in
    let p_minus_qshift = List.map2 (lxor) p_red q_shifted in
(*addition and subtraction are the same operation since we are over a field of characterstic 2*)
    remainder_euclidean_division_polynomials p_minus_qshift q_red;;
(*WARNING: since leading 0s are eliminated the size of the resulting list in not at all guarenteed to be m, 
   it may be any integer less than m*)

let gcd (p:polynomial) (q:polynomial) = 
  (*Applies Euclid's algorithm to polynomials in order to find the gcd*)
  let rec aux_gcd p1 p2 =
    (*p1 should have degree more than or equal the degree of p2*)
    match remainder_euclidean_division_polynomials p1 p2 with
    |[] -> p2
    |r -> aux_gcd p2 r
  in
  if (List.length p) > (List.length q) 
    then aux_gcd p q
    else aux_gcd q p


let normalise_lengths p q = 
  let p_len = List.length p and q_len = List.length q in
  if p_len < q_len then
    (List.init (q_len - p_len) (fun _ -> 0) @ p, q)
  else
    (p, List.init (p_len - q_len) (fun _ -> 0) @ q);;


let print_polynomial (p:polynomial) = 
  let deg_p =  (List.length p) - 1 in
      Printf.printf "";
      List.iteri (fun j i -> Printf.printf "%dX^%d, " i (deg_p - j)) p;
      Printf.printf "\n";;


let add_polynomials (p:polynomial) (q:polynomial) =
  let (p_norm, q_norm) = normalise_lengths p q in
  List.map2 (lxor) p_norm q_norm;;


let rec square_polynomial (p:polynomial) =
  match p with
  |[] -> []
  |coeff::res -> 0 :: coeff :: (square_polynomial res);;

let derivative (p:polynomial) =
  let rec annul_coeff_by_parity q=
    match q with
      |c1::_::res -> c1::0::(annul_coeff_by_parity res)
      |a -> a
  in 
  p |> List.rev |> List.tl |> annul_coeff_by_parity |> List.rev;;


let berlekamp (p:polynomial) 
(*We only want to check whether or not p is irreducible in F2*)
  = let deg = List.length p - 1 in
  let sp_X_degree d =
    (* the linear map that sends a polynomial Q(x) to Q(x^2) mod P*)
    let q = 1 :: List.init d (fun _ -> 0) in
    let q_squared_minus_q = add_polynomials (square_polynomial q) q in
    let provisional_result = remainder_euclidean_division_polynomials q_squared_minus_q p in
    let (res, _) = normalise_lengths provisional_result p in
    match res with
    | 0::result -> result
    | _ -> raise (WrongDimension (List.length res))
  in
  let sp_matrix = List.init deg sp_X_degree in
  Matrices.print_matrix sp_matrix;
  let rank = Matrices.rank sp_matrix in
  Printf.printf "Rank: %d\n" rank;
  deg - rank;;


let check_irreducibility (p:polynomial) =
  let d = derivative p in
  print_polynomial d;
  let g = gcd p d in
  print_polynomial g;
  match g with
  |[] |_::[] ->
    false
  |_ ->
    let num_irr_factors = berlekamp p in
    if num_irr_factors = 1 then
      true
    else
      false;;
