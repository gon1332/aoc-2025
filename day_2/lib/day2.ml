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
