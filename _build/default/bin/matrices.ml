type 'a matrix = 'a list list (*each sublist is of equal size*)

exception NonzeroAfterPivot
exception FlatMatrix
exception DisallowedValue

let rec print_matrix mat =
  match mat with
  |[] -> ()
  |l::res ->
    Printf.printf "[";
    List.iter (fun i -> Printf.printf "%d; " i) l;
    Printf.printf "]\n";
    print_matrix res;;


let rec transpose mat = 
  match mat with
  |[] -> []
  |l::[] -> List.map (fun e -> e::[]) l
  |l::res ->
    let t_res = transpose res in
    List.map2 (fun e l -> e::l) l t_res;;


let rec get_first_nonzero_index line = 
  match line with
  |[] -> None
  |0::res -> (
    match get_first_nonzero_index res with
    |None -> None
    |Some index -> Some (index + 1)
    )
  |_::_ -> Some 0;;


let rec nonzero_line_and_remaining_matirx mat = 
  match mat with
  |[] -> ([],[])
  |v::res_mat ->
    match v with
    |0::_ -> let (u, m) = nonzero_line_and_remaining_matirx res_mat in (u, v::m)
    |_ -> (v, res_mat);;


let remove_first_col mat = 
  List.map (fun l -> List.tl l) mat;;

let add_col_0s mat = 
  List.map (fun l ->  0::l) mat;;


let rec gauss_pivot mat = 
  match nonzero_line_and_remaining_matirx mat with
  |(v,[]) -> [v]
  |([],_) -> (match mat with
    |[]::_ -> mat
    |_ -> mat |> remove_first_col |> gauss_pivot |> add_col_0s
    )
  |(nonzero, res_mat) ->
    let anull_column row = 
      match row with
      |0::_ -> row
      |1::_ -> List.map2 (lxor) nonzero row
      |_ -> raise DisallowedValue
    in
    nonzero :: ((List.map anull_column res_mat) |> remove_first_col |> gauss_pivot |> add_col_0s);;

let rec is_0_vector l = 
  match l with
  |[] -> true
  |0::res -> is_0_vector res
  |_ -> false;;


let rank mat =
  let rec count_nonzero_lines mat =
    match mat with
    |[]->0
    |v::res -> 
      if is_0_vector v then 0
      else 1 + (count_nonzero_lines res)
  in 
  let g = mat |> gauss_pivot in
  Printf.printf "\nPivoted :\n";
  print_matrix g;
  g |> count_nonzero_lines;;
