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

module Node = struct
    type 'a t = One of 'a | Many of 'a t list

    let flatten lst = 
        let rec aux_flatten acc lst = match lst with
        | [] -> acc
        | h::t -> match h with
            | One  value -> aux_flatten (value::acc) t 
            | Many values -> aux_flatten acc (values@t) 
        in
   List.rev (aux_flatten [] lst)
end

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

module RLE = struct
    type 'a t = 
        | One of 'a
        | Many of int * 'a

    let encode lst = 
        let rec aux acc dupes_count last_item = function
            | [] -> (* Handle the last item after the list is fully processed *)
                if dupes_count = 1 then (One last_item :: acc) 
                else (Many (dupes_count, last_item) :: acc)
            | h::t -> if last_item = h then aux acc (dupes_count + 1) h t else
                if dupes_count = 1 then aux ((One last_item)::acc) 1 h t else aux ((Many (dupes_count, last_item))::acc) 1 h t 
        in
        match lst with
        | [] -> []
        | head::tail -> List.rev (aux [] 1 head tail) 

    let decode lst = 
        let rec aux_repeat acc count item = if count = 0 then acc else aux_repeat (item::acc) (count - 1) item in
        let rec aux acc = function 
            | [] -> acc
            | One value::t ->  aux (aux_repeat acc 1 value) t
            | Many (count,values)::t -> aux (aux_repeat acc count values) t
        in 
        List.rev (aux [] lst) 


    let print_modified_run_length_encoding = print_array ( fun a -> match a with
        | One value -> print_string "One value: " ;print_string value
        | Many (count,value) -> print_string "Many Values: "; print_int count;print_string value)
end

let duplicate lst = 
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (h::h::acc) t
    in
    aux [] (List.rev lst) 
 
let replicate lst count = 
    let rec aux_repeat acc count item = if count = 0 then acc else aux_repeat (item::acc) (count - 1) item in
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (aux_repeat acc count h) t
    in
    aux [] (List.rev lst) 

let drop lst count = 
    let rec aux acc aux_count = function
        | [] -> acc
        | h::t -> if aux_count = count then aux acc 1 t else aux (h::acc) (aux_count+1) t
    in
    List.rev (aux [] 1 lst) 

let split lst count = 
    let rec aux left_split right_split count = match right_split with
        | [] -> (List.rev left_split,  right_split)
        | h::t as l -> if count > 0 then aux (h::left_split) t (count-1) else (List.rev left_split, l)
    in 
    aux [] lst count 

let slice lst begin_index end_index = 
    let rec aux acc index = function
        | [] -> acc
        | h::t -> if index > end_index then aux acc (index + 1) []
                  else if index < begin_index then aux acc (index+1) t
                  else aux (h::acc) (index + 1) t
    in 
    List.rev (aux [] 0 lst) 

let rotate_left lst n = 
    let len_of_lst = List.length lst in
    let n = if len_of_lst = 0 then 0 else (n mod len_of_lst) in
    if n = 0 then lst else 
    let left, right = split lst n in
    right @ left

let remove_at lst i =
    if i = 0 then match lst with
    | [] -> []
    | _::t -> t
    else
    let left, right = split lst i in match right with
    | [] -> left
    | _::t -> left@t

(* Insert an Element at a Given Position Into a List *)    
let insert_at value index lst =
    if index = 0 then (value::lst) else 
    let length = List.length lst in
    if  index >= length then lst@[value] else 
    let rec aux value index lst acc = match lst with
        | [] -> [value]
        | h::t as l -> if index = 0 then (List.rev (value::acc))@l else aux value (index-1) t (h::acc) 
    in
    aux value index lst []

(* Create a List Containing All Integers Within a Given Range *)
let range a b =
    let rec aux a b acc =
        if a = b then a::acc else aux a (b-1) (b::acc)
    in
    if a > b then [] else aux a b [] 
