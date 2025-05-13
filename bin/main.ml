let rec last lst = match lst with
    | [] -> None
    | [ a ] -> Some a
    | _::t -> last t
    
let rec last_two = function 
    | [] -> None
    | [_] -> None
    | [a;b] -> Some (a,b)
    | _::t -> last_two t

let rec at lst n = match lst with
    | [] -> None 
    | h::t -> if  n = 0 then Some h else at t (n-1)

let length_list lst = 
    let rec aux_length_list lst acc = match lst with
    | [] -> acc
    | _::t -> aux_length_list t (acc + 1) in
    aux_length_list lst 0

let rev lst = 
    let rec aux_rev acc = function
        | [] -> acc
        | h::t -> aux_rev (h::acc) t in
    aux_rev lst []

let rec print_array f lst = match lst with
    | [] -> ()
    | h::t -> f h; print_string " " ;print_array f t  

let rec print_matrix f = function 
    | [] -> ()
    | h::t -> print_char '[';print_array f h; print_char ']'; print_matrix f t

let print_int_array = print_array print_int
(* let print_char_array = print_array print_char *)
let print_string_array = print_array print_string

(* let print_char_matrix = print_array print_char *)
(*let print_int_matrix = print_matrix print_int*)
let print_string_matrix = print_matrix print_string

let is_palindrome lst = 
    (List.rev lst) = lst

type 'a node = One of 'a | Many of 'a node list

let flatten lst = 
    let rec aux_flatten acc lst = match lst with
    | [] -> acc
    | h::t -> match h with
        | One  value -> aux_flatten (value::acc) t 
        | Many values -> aux_flatten acc (values@t) 
    in
   List.rev (aux_flatten [] lst)
 
let eliminate_dupes lst = List.rev (List.fold_left (fun acc a ->  if List.mem a acc then acc else a :: acc) [] lst) 

let pack lst = 
    let rec aux acc sub_list_dupes last_item = function
        | [] -> sub_list_dupes::acc
        | h::t -> if last_item = h then aux acc (h::sub_list_dupes) h t else  aux (sub_list_dupes::acc) (h::[]) h t
    in
    match lst with
    | [] -> []
    | head::tail -> List.rev (aux [] [head] head tail) 
    
let encode lst = 
    let packed_list = pack lst in 
    let rec aux acc = function
        | [] -> acc
        | h::t -> let length_of_sublist = List.length h in aux ((length_of_sublist,List.nth h 0)::acc) t
    in 
    List.rev (aux [] packed_list) 

let print_run_length_encoding = print_array (fun (a,b) -> print_char '('; print_int a; print_char ' ';print_string b; print_char ')')

let () = 
    let lst = [1;23;4;5;6] in
    let last_value = last lst in
    let last_two_values = last_two lst in
    let nth_element = at lst 3 in
    let length_of_list = length_list lst in
    let rev_list = rev lst in
    let palindrome_test_list = ["x"; "a"; "m"; "a"; "x"] in
    let a_node = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
    let a_node_flatten = flatten a_node in
    let compression_lst_test = ["a"; "a"; "a"; "a"; "b"; "c"; "kc"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
    let compressed_lst = eliminate_dupes compression_lst_test in
    let sublist_duplicates_test = pack compression_lst_test in
    let run_length_encoded_list = encode compression_lst_test in
    (*-----------------------------------------------------*)
    let _ = match last_value with
    | None -> print_endline "the list was empty"
    | Some value -> print_endline (string_of_int value) in
    let _ = match last_two_values with
    | None -> print_endline "the list was empty"
    | Some (value1,value2)-> print_endline (string_of_int value1); print_endline (string_of_int value2) in
    let _ = match nth_element with
    | None -> print_endline "the list was empty"
    | Some (value) -> print_endline (string_of_int value) in
    let _ = print_endline (string_of_int length_of_list) in
    let _ = print_int_array rev_list in
    let _ = print_endline "" in
    let _ = print_endline (string_of_bool (is_palindrome palindrome_test_list)) in
    let _ = print_string_array a_node_flatten in
    let _ = print_endline "" in
    let _ = print_string_array compressed_lst in
    let _ = print_endline "" in
    let _ = print_string_matrix sublist_duplicates_test; print_endline "" in
    let _ = print_run_length_encoding run_length_encoded_list in
    print_endline "" 
