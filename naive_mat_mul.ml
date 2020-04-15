module T = Domainslib.Task

let num_domains =
    try
        Sys.argv.(2) |> int_of_string
    with _ ->
        2

let size =
    try
        Sys.argv.(1) |> int_of_string
    with _ ->
        1024

(* let zip = fun list_A list_B ->
    assert ((List.length list_A) = (List.length list_B));
    let rec zip' = fun acc list_A list_B ->
        match (list_A, list_B) with
        | ([],[]) -> acc
        | ((_::_, [])|([], _::_)) -> failwith "Uneven lists"
        | (hd1::tl1, hd2::tl2) -> zip' ((hd1, hd2) :: acc) tl1 tl2 in
    zip' [] list_A list_B

let range = fun init final ->
    List.init (final + 1) (fun x -> x*1)
    |> List.filter (fun x -> if x < init then false else true) *)

let range initial final = Array.init final (fun x -> x)
    
(* eg. 
    let matrix = [[1;2;3];[4;5;6];[7;8;9]];;
    // [[1;2;3];[4;5;6];[7;8;9]]
    let result = first_elems matrix;; 
    // [1;4;7] *)
(* let first_elems = fun matrix ->
    List.map (fun a -> List.hd a) matrix  *)
(* eg. 
    let matrix = [[1;2;3];[4;5;6];[7;8;9]];;
    // [[1;2;3];[4;5;6];[7;8;9]]
    let result = rest_elems matrix;; 
    // [[2;3]; [5;6]; [8;9]] *)
(* let rest_elems = fun matrix ->
    List.map (fun a -> List.tl a) matrix *)

let transpose = fun x ->
    let num_rows = Array.length x in
    let num_columns = Array.length @@ Array.get x 0 in
    let temp_arr = Array.make (num_rows) (Array.make (num_columns) 0) in
    for i = 0 to (num_rows - 1) do
        for j = 0 to (num_columns - 1) do
            temp_arr.(j).(i) <- x.(i).(j);
        done;
    done;
    temp_arr

(* let get_row_vector matrix index =
    List.nth matrix index

let get_column_vector matrix index =
    let rec loop = fun matrix index ->
        match index with
        | 0 -> first_elems matrix
        | _ -> loop (rest_elems matrix) (index - 1) in
    loop matrix index

let split = fun lst n ->
    let rec split' = fun left lst n ->
        match n with
        | 0 -> (left |> List.rev, lst)
        | _ -> split' (List.hd lst :: left) (List.tl lst) (n - 1) in
    split' [] lst n *)

(* let matrix_chunker = fun arr num_chunks -> 
    let arr_len = Array.length arr in

    let rec chunky = fun acc lst num_chunks ->
        match (List.length lst) < num_chunks  with
        | true -> if List.length lst = 0 then 
                    List.rev acc 
                    else lst :: acc |> List.rev 
        | false -> let (fst,snd) = split lst num_chunks in chunky (fst :: acc) snd num_chunks
    in
    chunky [] lst num_chunks *)

let unit_matrix_op = fun row_vector_A column_vector_B -> 
    (* 
    1 - multiply zipped elements
    2 - add the tuples 
    if matrix_A = [
        [1;2;3];
        [4;5;6];
        [7;8;9]
    ] 
    and matrix_B = [
            [9;8;7];
            [6;5;4];
            [3;2;1]
    ]

    eg. unit_reduce_op [1;2;3] [9;6;3] 
        = [(1,9); (2,6); (3,3)] 
        = [9; 12; 9]
        = 9 + 12 + 9
        = 30
    *)
    let arr_len = Array.length row_vector_A in
    let f i = row_vector_A.(i) * column_vector_B.(i) in
    let sum = ref 0 in
    for i = 0 to pred arr_len do
        sum := !sum + (f i);
    done;
    !sum

    (* Array.fold_left (fun acc i -> acc + (f i)) 0 (range arr_len) *)
    (* zip row_vector_A column_vector_B
    |> fun lst -> 
        List.map (fun (a, b) -> a * b) lst
    |> List.fold_left (fun acc x -> acc + x) 0 *)

(* let worker_matrix_op pool matrix_A matrix_B = () *)
    (* 
    1 - chunk row_matrix_A according to chunk size
    2 - use that chunk with matrix_B to calculate 
        2.1 Do we need to chunk matrix_B -- No we don't
    3 - map row vectors from A to column vectors from B   *)
    (* let chunk_multiplier = (List.length matrix_A) / chunk in
    let index_range = range 1 chunk |> List.map (fun x -> x * chunk_multiplier) in *)

    (* let matrix_B_T = transpose matrix_B in *)
    (* let worker_task = fun matrix_A_chunk matrix_B_T ->
        let each_row_vector = fun row_vector -> 
        (* int list *)
            List.map (fun column_vector -> unit_matrix_op row_vector column_vector) matrix_B_T in 
        List.fold_left (fun acc row_vector -> acc @ (each_row_vector row_vector)) (List.hd matrix_A_chunk |> each_row_vector) (List.tl matrix_A_chunk) in
    let promise_list = fun matrix_A -> 
        matrix_chunker matrix_A chunk_size
        |> List.map (fun matrix_A_chunk -> T.async pool (fun _ -> worker_task matrix_A_chunk matrix_B_T)) in
    promise_list matrix_A *)
    (* let promise_arr =  *)

(* let par_mat_mul pool matrix_A matrix_B =
    let (r_A, c_A) = (Array.length matrix_A, Array.length @@ matrix_A.(0)) in
    let (r_B, c_B) = (Array.length matrix_B, Array.length @@  matrix_B.(0)) in
    assert (c_A = r_B);
    (* let chunk_size = List.length matrix_A |> fun n -> n / num_domains in
    matrix_op pool chunk_size matrix_A matrix_B
    |> fun promise_list ->
        List.fold_left (fun acc x -> (T.await pool x) :: acc) [] promise_list
    |> List.rev *)
    let matrix_C = Array.make_matrix r_A c_B 0 in
    let matrix_B_T = transpose matrix_B in
    Array.iteri (r_index_A row_A -> 
        Array.iteri (r_index_B_T row_B ->

            )) *)

let mat_mul domain_pool matrix_A matrix_B = 
    let matrix_BT = transpose matrix_B in
    let (rA, cA) = (Array.length matrix_A, Array.length @@ matrix_A.(0)) in
    let (rBT, cBT) = (Array.length matrix_BT, Array.length @@ matrix_BT.(0)) in
    assert (cA = rB);
    let matrix_C = Array.make_matrix rA cB 0 in
    Array.iteri (fun index_A row_A -> 
        Array.iteri (fun index_B_T row_B_T ->
        matrix_C.(index_A).(index_B_T) <- unit_matrix_op row_A row_B_T;) matrix_B_T;) matrix_A;
                
let () =
  let domain_pool = T.setup_pool ~num_domains:num_domains in
  let m1 = Array.make_matrix size size (Random.int 100) in
  let m2 = Array.make_matrix size size (Random.int 100) in 
  let mat = mat_mul domain_pool m1 m2 in
  (* List.iter (fun i -> 
    List.iter ( fun j ->  Printf.printf " %d " j) i; print_endline "" ) mat;
    print_endline ""; *)
  T.teardown_pool domain_pool