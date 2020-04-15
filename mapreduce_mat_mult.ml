create_map = fun matrix ->
    list of map_elements

let get_element = fun mat_str row col mat_val -> ((mat_index, row, col), mat_val)

let create_map = fun matrix -> 
    Array.mapi (fun r_index row ->
        Array.mapi (fun c_index col -> ) row ) matrix