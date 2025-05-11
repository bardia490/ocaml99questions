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

let rec print_array = function
    | [] -> ()
    | h::t -> print_array t; print_int h 

let () = 
    let lst = [1;23;4;5;6] in
    let last_value = last lst in
    let last_two_values = last_two lst in
    let nth_element = at lst 3 in
    let length_of_list = length_list lst in
    let rev_list = rev lst in
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
    let _ = print_array rev_list in
    let _ = print_endline "" in
    ()
