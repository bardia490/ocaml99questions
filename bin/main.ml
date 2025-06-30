let () = 
    let lst = [1;23;4;5;6] in
    let last_value = Lib.last lst in
    let last_two_values = Lib.last_two lst in
    let nth_element = Lib.at lst 3 in
    let length_of_list = Lib.length_list lst in
    let rev_list = Lib.rev lst in
    let palindrome_test_list = ["x"; "a"; "m"; "a"; "x"] in
    let a_node =  [Lib.Node.One "a"; Lib.Node.Many [Lib.Node.One "b"; Lib.Node.Many [Lib.Node.One "c" ;Lib.Node.One "d"]; Lib.Node.One "e"]] in
    let a_node_flatten = Lib.Node.flatten a_node in
    let compression_lst_test = ["a"; "a"; "a"; "a"; "b"; "c"; "kc"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
    let compressed_lst = Lib.eliminate_dupes compression_lst_test in
    let sublist_duplicates_test = Lib.pack compression_lst_test in
    let length_encoded_list = Lib.encode compression_lst_test in
    let modified_length_encoded_list = Lib.RLE.encode compression_lst_test in
    let length_decoded_list = Lib.RLE.decode modified_length_encoded_list in
    let duplicated_list = Lib.duplicate ["a"; "b"; "c"; "c"; "d"] in
    let replicated_list = Lib.replicate ["a"; "b"; "c"; "c"; "d"] 3 in
    let dropped_list = Lib.drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 in
    let (left_splitted_list,right_splitted_list) = Lib.split ["a"; "b"; "c"; "d"] 1 in
    let sliced_list = Lib.slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 in
    let rotated_list = Lib.rotate_left ["a"; "b"; "c"; "d"] 5 in
    let second_item_removed_list = Lib.remove_at ["a"; "b"; "c"; "d"] 0 in
    let insert_item_list = Lib.insert_at "hello" 10 ["a"; "b"; "c"; "d"] in
    let ranged_list = Lib.range 4 9 in
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
    let _ = Lib.print_int_array rev_list in
    let _ = print_endline "" in
    let _ = print_endline (string_of_bool (Lib.is_palindrome palindrome_test_list)) in
    let _ = Lib.print_string_array a_node_flatten in
    let _ = print_endline "" in
    let _ = Lib.print_string_array compressed_lst in
    let _ = print_endline "" in
    let _ = Lib.print_string_matrix sublist_duplicates_test; print_endline "" in
    let _ = Lib.print_run_length_encoding length_encoded_list; print_endline "" in
    let _ = Lib.RLE.print_modified_run_length_encoding modified_length_encoded_list; print_endline "" in
    let _ = Lib.print_string_array length_decoded_list; print_endline "" in
    let _ = Lib.print_string_array duplicated_list in
    let _ = Lib.print_string_array replicated_list; print_endline "" in
    let _ = Lib.print_string_array dropped_list ; print_endline "" in
    let _ = print_string "left array: " in let _ = Lib.print_string_array left_splitted_list in let _ = print_string "right array: " in let _ = Lib.print_string_array right_splitted_list in let _ = print_endline "" in
    let _ = Lib.print_string_array sliced_list;  print_endline "" in
    let _ = Lib.print_string_array rotated_list in let _ =  print_endline "" in 
    let _ = Lib.print_string_array second_item_removed_list in let _ = print_endline "" in
    let _ = Lib.print_string_array insert_item_list in let _ =  print_endline "" in
    let _ = Lib.print_int_array ranged_list in  print_endline ""
