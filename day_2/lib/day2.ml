module Day2 = struct
        let range_from_str str =
                Scanf.sscanf_opt str "%u-%u" (fun l r -> (l, r))

        (* Assumes l < r *)
        let sum_of_invalid_ids = function
                | Some (l, r) ->
                        let sum = ref 0 in
                        for i = l to r do
                                let id = string_of_int i in
                                match String.length id mod 2 with
                                | 0 ->
                                        let len_sub = String.length id / 2 in
                                        let (f, s) = (Str.string_before id len_sub, Str.string_after id len_sub) in
                                        if f = s then
                                                sum := !sum + i
                                        else ()
                                | _ -> ()
                        done;
                        !sum;
                | None -> 0

        (* Similar to sum_of_invalid_ids but recognizes every repeating pattern *)
        let sum_of_invalid_ids_2 = function
                | Some (l, r) ->
                        let sum = ref 0 in
                        for i = l to r do
                                let id = string_of_int i in
                                let len_sub = String.length id / 2 in
                                let c_idx = ref 1 in
                                let quit_loop = ref false in (* TODO: Replace this with recursion *)
                                        while not !quit_loop && !c_idx <= len_sub do
                                                let partial = String.make !c_idx '_' in
                                                let res = Str.global_replace (Str.regexp_string (Str.string_before id !c_idx)) partial id in
                                                        (* Printf.printf "s/%s/%s/g in %s => %s\n" (Str.string_before id !c_idx) partial id res; *)
                                                        if res = String.make (String.length id) '_' then
                                                        begin
                                                                quit_loop := true;
                                                                sum := !sum + i
                                                        end
                                                        else incr c_idx;
                                        done
                        done;
                        !sum;
                | None -> 0
end

let%test _ = Day2.range_from_str "0-1" = Some (0, 1)
let%test _ = Day2.range_from_str "1-0" = Some (1, 0)
let%test _ = Day2.range_from_str "a-0" = None
let%test _ = Day2.range_from_str "0-b" = None
let%test _ = Day2.range_from_str "a-b" = None
let%test _ = Day2.range_from_str "01" = None

let%test _ = Day2.sum_of_invalid_ids None = 0
let%test _ = Day2.sum_of_invalid_ids @@ Some (0, 1) = 0
let%test _ = Day2.sum_of_invalid_ids @@ Some (11, 11) = 11
let%test _ = Day2.sum_of_invalid_ids @@ Some (11, 22) = 33
let%test _ = Day2.sum_of_invalid_ids @@ Some (0, 40) = 66

let%test _ = Day2.range_from_str "01" |> Day2.sum_of_invalid_ids = 0
let%test _ = Day2.range_from_str "0-1" |> Day2.sum_of_invalid_ids = 0
let%test _ = Day2.range_from_str "10-11" |> Day2.sum_of_invalid_ids = 11
let%test _ = Day2.range_from_str "10-30" |> Day2.sum_of_invalid_ids = 33

let%test _ = Day2.sum_of_invalid_ids_2 @@ None = 0
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (0, 1) = 0
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (11, 11) = 11
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (111, 111) = 111
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (1111, 1111) = 1111
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (0, 40) = 66
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (121, 121) = 0
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (565656, 565656) = 565656
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (11, 22) = 11 + 22
let%test _ = Day2.sum_of_invalid_ids_2 @@ Some (95, 115) = 99 + 111
