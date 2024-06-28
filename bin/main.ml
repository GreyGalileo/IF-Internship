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
