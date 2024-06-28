(*let find_irreducible_polynomial deg =
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
  three_term_test_loop 1;;*)


(*Field2m.print_inverse [1; 1; 1; 1; 1; 1;];;*)


let rec list_irreducibles n =
  if n <= 1 then
    ()
  else
    let p = Polynomials.irreducible_polynomial_degree n in
    match p with
    |Some q -> 
      Printf.printf "\nIrreducible polynomial of degree %d :" n;
      Polynomials.print_polynomial q;
      list_irreducibles (n-1)
    |None -> 
      Printf.printf "\nNo irreducible polynomial of degree %d found over F2m\n" n;
      list_irreducibles (n-1);;
    

list_irreducibles 20;;
