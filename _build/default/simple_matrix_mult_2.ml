module T = Domainslib.Task

let num_domains = try Sys.argv.(2) |> int_of_string with _ -> 2
let size = try Sys.argv.(1) |> int_of_string with _ -> 1024
let domain_pool = T.setup_pool ~num_domains:num_domains

(* let transpose = fun x ->
    let num_rows = Array.length x in
    let num_columns = Array.length x.(0) in
    let temp_arr = Array.copy x in
    for i = 0 to pred num_rows do
        for j = 0 to pred num_columns do
            temp_arr.(j).(i) <- x.(i).(j);
        done;
    done;
    temp_arr *)

(* let unit_matrix_op = fun row_vector_A column_vector_B -> 
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
    !sum *)
    let slice a (x, xlen) (y, ylen) = 
        let first_entry = Array.sub a.(x) y ylen in
        let res_matrix = Array.make xlen first_entry in
        for i = x+1 to x+xlen-1 do
            res_matrix.(i-x) <- Array.sub a.(i) y ylen
        done;
        res_matrix

    (* merge 4 matrices into one *)
    (* no dimension checking; can be applied to 2 matrix with the other two dim-0 matrices *)
    let merge_matrix c00 c10 c01 c11 = 
        let x0 = Array.length c00 and x1 = Array.length c10 
            and y0 = Array.length c00.(0) and y1 = Array.length c01.(0) in
        let x = x0 + x1 and y = y0 + y1 in
        let res_matrix = Array.make_matrix x y 0. in
        for i = 0 to x0 - 1 do
            res_matrix.(i) <- Array.append c00.(i) c01.(i)
        done;
        for i = 0 to x1 - 1 do
            res_matrix.(x0 + i) <- Array.append c10.(i) c11.(i)
        done;
        res_matrix

    (* add two matrices *)
    let add m1 m2 = 
        let x = Array.length m1 and y = Array.length m1.(0) in
        if x <> Array.length m2 || y <> Array.length m2.(0) then failwith "incompatible matrices!"
        else
           let res_matrix = Array.make_matrix x y 0. in
           for i = 0 to x-1 do
                for j = 0 to y-1 do
                    res_matrix.(i).(j) <- m1.(i).(j) +. m2.(i).(j)
                done
            done;
            res_matrix

    (* scale a matrix *)
    let scale m k = 
        let x = Array.length m and y = Array.length m.(0) in
        let res_matrix = Array.make_matrix x y 0. in
        for i = 0 to x-1 do
            for j = 0 to y-1 do
                res_matrix.(i).(j) <- m.(i).(j) *. k
            done
        done;
        res_matrix
    
    (* matrix subtraction *)
    let subtract m1 m2 = 
        add m1 (scale m2 (-1.))
    
    (* straight-forward multiplication *)
    let mult m0 m1 = 
        let x0 = Array.length m0 and y0 = Array.length m0.(0) and
            x1 = Array.length m1 and y1 = Array.length m1.(0) in
        if y0 <> x1 then failwith "incompatible matrices!"
        else
            let res_matrix = Array.make_matrix x0 y1 0. in
            for i = 0 to x0 - 1 do
                for j = 0 to y1 - 1 do
                    for k = 0 to y0 - 1 do
                        res_matrix.(i).(j) <- res_matrix.(i).(j) +. m0.(i).(k) *. m1.(k).(j)
                    done
                done
            done;
            res_matrix

    (* Strassen's Algorithm *)
    
    (* base case: direct multiplication for dim2 matrices *)
    let mult_dim2 m0 m1 = 
        let res = Array.make_matrix 2 2 0. in
        res.(0).(0) <- m0.(0).(0) *. m1.(0).(0) +. m0.(0).(1) *. m1.(1).(0);
        res.(1).(0) <- m0.(1).(0) *. m1.(0).(0) +. m0.(1).(1) *. m1.(1).(0);
        res.(0).(1) <- m0.(0).(0) *. m1.(0).(1) +. m0.(0).(1) *. m1.(1).(1);
        res.(1).(1) <- m0.(1).(0) *. m1.(0).(1) +. m0.(1).(1) *. m1.(1).(1);
        res
    
    (* util function: slice a matrix into equal 4 pieces *)
    let slice_four a =  
        let n = Array.length a / 2 in
        let a11 = T.async domain_pool (fun _ -> slice a (0,n) (0,n)) and 
            a21 = T.async domain_pool (fun _ -> slice a (n,n) (0,n)) and 
            a12 = T.async domain_pool (fun _ -> slice a (0,n) (n,n)) and 
            a22 = T.async domain_pool (fun _ -> slice a (n,n) (n,n)) in
        T.await domain_pool a11, T.await domain_pool a12, T.await domain_pool a21, T.await domain_pool a22
    
    (* find next power of 2; assuming 64-bit machine *)
    let next_power_2 n = 
        let m = n - 1 in
        let n = m lor (m lsr 1) in
        let m = n lor (n lsr 2) in
        let n = m lor (m lsr 4) in
        let m = n lor (n lsr 8) in
        let n = m lor (m lsr 16) in
        let m = n lor (n lsr 32) in
        m + 1

    (* pad a to square dimensions *)
    let padding a n = 
        let x = Array.length a and y = Array.length a.(0) in
        let res = Array.make_matrix n n 0. in
        for i = 0 to x-1 do
            res.(i) <- Array.append a.(i) (Array.make (n-y) 0.)
        done;
        res

    (* Strassen's Algorithm *)
    
    let rec aux_mult_strassen a b =
        let async = fun x ->
            T.async domain_pool x in
        let await = fun x ->
            T.await domain_pool x in
        let n = Array.length a in
        if n <= 2 then mult_dim2 a b
        else
           let (a11,a12,a21,a22) = slice_four a and
           (b11,b12,b21,b22) = slice_four b in
           let m1 = async @@ fun () -> aux_mult_strassen (add a11 a22) (add b11 b22) and
                m2 = async @@ fun () -> aux_mult_strassen (add a21 a22) b11 and
                m3 = async @@ fun () -> aux_mult_strassen a11 (subtract b12 b22) and
                m4 = async @@ fun () -> aux_mult_strassen a22 (subtract b21 b11) and
                m5 = async @@ fun () -> aux_mult_strassen (add a11 a12) b22 and
                m6 = async @@ fun () -> aux_mult_strassen (subtract a21 a11) (add b11 b12) and
                m7 = async @@ fun () -> aux_mult_strassen (subtract a12 a22) (add b21 b22) in
           let c11 = add (subtract (add (await m1) (await m4)) (await m5)) (await m7) and
                c12 = add (await m3) (await m5) and
                c21 = add (await m2) (await m4) and
                c22 = add (add (subtract (await m1) (await m2)) (await m3)) (await m6) in
           merge_matrix c11 c21 c12 c22
    
    let mult_strassen a b = 
        let x0 = Array.length a and y0 = Array.length a.(0) 
        and x1 = Array.length b and y1 = Array.length b.(0) in
        if y0 <> x1 then failwith "incompatible matrices!"
        else
            let n = next_power_2 (max (max x0 y0) y1) in
            let pa = padding a n and pb = padding b n 
            in slice (aux_mult_strassen pa pb) (0,x0) (0,y1)

let _ =
    let m1 = Array.init size (fun _ -> Array.init size (fun _ -> Random.float 100.0))
        and m2 = Array.init size (fun _ -> Array.init size (fun _ -> Random.float 100.0)) in

    let _m3 = mult_strassen m1 m2 in
    (* Array.iter (fun row ->
        Array.iter (fun row_el -> Printf.printf " %f " row_el) row; print_endline "" ) _m3;
    print_endline ""; *)
    T.teardown_pool domain_pool;