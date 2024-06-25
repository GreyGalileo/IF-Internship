(*
Here we define the size of F2^m, 
i.e the dimension of our field over F2, 
or the number of elements in our field log2

We also give an explicit form of the polynomial for which the elements of the field are roots,
i.e. the polynomial which generates the ideal for the quotient field
*)
(* Polynomials (as well as members of F2^m) will be represented as lists of 1s and 0s, coefficients of highest degree first *)
(* F2-linear polynomials over F2^m will later be represented as functions (i.e. endomorphisms F2^m -> F2^m)*)
(*open Polynomials*)


let m = 6;;

type field2m = int list;; (*should alays be of size m*)
(*We will essentially treat this as tuples of m entries from F2, which is anyways isomorphic to the field*)

exception DisallowedValue (*when a memeber of field2m contains an entry not in {0,1}*);;

let ideal_polynomial (*Polynomial which generates the ideal over which the quotient field is defined*)
  = let arr = Array.make (m+1) 0 in
  arr.(0) <- 1;
  arr.(1) <- 1;
  arr.(m) <- 1;
  List.rev (Array.to_list arr);;(*Polynomial x^m + x + 1 = 0 over F2m*)

let print_f2m (e:field2m) = 
  Printf.printf "(";
  List.iter (fun i -> Printf.printf "%d, " i) e;
  Printf.printf ")\n";;


let rec eliminate_leading_0s (p:int list) = 
  match p with
  | 0 :: res -> eliminate_leading_0s res
  | _ -> p;;

let rec remainder_euclidean_division_polynomials: int list -> int list -> int list
  = fun p q ->  (*remainder of the euclidean division of p by q*)
  let p_red = eliminate_leading_0s p and q_red = eliminate_leading_0s q in
  let p_len = List.length p_red and q_len = List.length q_red in
  if p_len < q_len then p_red
  else 
    let q_shifted = q_red @ List.init (p_len - q_len) (fun _ -> 0) in
    let p_minus_qshift = List.map2 (lxor) p_red q_shifted in
(*addition and subtraction are the same operation since we are over a field of characterstic 2*)
    remainder_euclidean_division_polynomials p_minus_qshift q_red;;
(*WARNING: since leading 0s are eliminated the size of the resulting list in not at all guarenteed to be m, 
   it may be any integer less than m*)

let complete_to_length_m p = List.init (m - List.length p) (fun _ -> 0) @ p;;


let add_field2m: field2m -> field2m -> field2m 
   = fun a b -> List.map2 (lxor) a b;;

let mult_field2m: field2m -> field2m -> field2m 
  = fun a b ->
    let shift_up p n = p @ List.init n (fun _ -> 0) in
    let rec multiply_polynomials (degree:int) (p1 : int list) (p2 : int list) =
      (*iterates over p1 and shifts p2 by the degree*)
      match p1 with
      | [] -> assert (degree = -1); complete_to_length_m []
      | 0::res -> multiply_polynomials (degree-1) res p2
      | 1::res -> 
        let p2_shifted = (shift_up p2 degree) in 
        let p2_multiplied = (remainder_euclidean_division_polynomials p2_shifted ideal_polynomial) |> complete_to_length_m in
        add_field2m p2_multiplied (multiply_polynomials (degree-1) res p2)
      | _ -> raise DisallowedValue
    in
  multiply_polynomials (m-1) a b;;


let square_field2m: field2m-> field2m
  = fun a ->
    let rec full_square (b:field2m) =
      match b with
      |[] -> []
      |entry::res -> 0 :: entry :: (full_square res)
    in
    remainder_euclidean_division_polynomials (full_square a) ideal_polynomial |> complete_to_length_m;;


let omegasquared =  
  let arr = Array.make (m) 0 in
  arr.(2) <- 1;
  List.rev (Array.to_list arr);;

let p =   
  let arr = Array.make (m) 0 in
  arr.(2) <- 1;
  arr.(4) <- 1;
  arr.(5) <- 1;
  List.rev (Array.to_list arr);;


print_f2m ideal_polynomial;;
print_f2m omegasquared;;
print_f2m p;;
Printf.printf "\nAddition:\n";;
print_f2m (add_field2m p omegasquared);;
Printf.printf "\nMultiplication:\n";;
print_f2m (mult_field2m p omegasquared);;
Printf.printf "\nSquare:\n";;
print_f2m (square_field2m p);;
print_f2m (square_field2m omegasquared);;