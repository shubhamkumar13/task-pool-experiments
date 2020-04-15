(* // PE(i , j)
k := (i + j) mod N;
a := a[i][k];
b := b[k][j];
c[i][j] := 0;
for (l := 0; l < N − 1; l++) {
    c[i][j] := c[i][j] + a * b;
        concurrently {
            send a to PE(i, (j + N − 1) mod N);
            send b to PE((i + N − 1) mod N, j);
        } with {
            receive a' from PE(i, (j + 1) mod N);
            receive b' from PE((i + 1) mod N, j );
        }
    a := a';
    b := b';
} *)

module T = Domainslib.Task

let pool = T.setup_pool ~num_domains:4
let size = try Sys.argv.(1) with _ -> 1024

let _ =
    let m1 = Array.init size (fun _ -> 
                (Array.init size (fun _ -> Random.int 100))) and
        m2 = Array.init size (fun _ -> 
                (Array.init size (fun _ -> Random.int 100))) in
    
    let m3 = mat_mul pool m1 m2 in
    T.teardown_pool pool;