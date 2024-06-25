let find_irreducible_polynomial deg =
  let arr = Array.make (deg+1) 0 in
  arr.(0) <- 1;
  arr.(deg) <- 1;
  let rec three_term_test_loop n =
    if n >= deg then
      None
    else
      begin
      arr.(n) <- 1;
      let p = (Array.to_list arr) in
      Printf.printf "\nPolynomial:\n";
      Polynomials.print_polynomial p;
      if Polynomials.check_irreducibility p then
        Some p
      else
        (arr.(n) <- 0; three_term_test_loop (n+1))
      end
  in
  three_term_test_loop 1;;



let ideal_polynomial = find_irreducible_polynomial 20;;
match ideal_polynomial with
|None -> Printf.printf "There is no 3-term irreducible polynomial for this degree\n"
|Some p -> Polynomials.print_polynomial p;;
