module Day3 = struct
        exception Too_Small_Bank of string

        let max_joltage bank =
                let rec doit bank_list cache =
                        match bank_list with
                        | _::[] -> Int64.of_int cache
                        | [] -> raise @@ Too_Small_Bank "The battery bank must include at least two batteries";
                        | num1::rest ->
                                let num2 = List.fold_left max 0 rest in
                                let this_joltage = num1 * 10 + num2 in
                                doit rest @@ if this_joltage > cache then this_joltage else cache
                in
                let bank_list = String.to_seq bank |> List.of_seq |> List.map (fun c -> int_of_char c - int_of_char '0') in
                doit bank_list 0

        let ultimate_joltage total bank =
                let bank_list = String.to_seq bank |> List.of_seq |> List.map (fun c -> int_of_char c - int_of_char '0') in
                let find_after after rep =
                try
                        (* Finds the maximum possible value in the available string
                           Shifts a window from left to right. The window tightens as we reach the end of the input *)
                        let window = List.take (List.length bank_list - (total - rep)) bank_list |> List.drop after in
                        let max_num = List.fold_left max 0 window in
                        let all_options = List.mapi (fun idx el -> (after + idx, el)) window in
                        let only_max = List.filter (fun (_, el) -> el = max_num) all_options in
                                List.hd only_max (* Of all the options return only the first one as it
                                                    has the most possibilities to form maximum joltage *)
                with e ->
                        match e with
                        | _ -> raise e
                in
                let rec assemble idx iter accum =
                        if iter > total then
                                let convert m n = Int64.add (Int64.mul m 10L) n in
                                List.fold_left (convert) 0L (List.rev accum)
                        else
                                let max_possible_idx, max_possible_num = find_after idx iter in
                                assemble (1 + max_possible_idx) (iter + 1) ((Int64.of_int max_possible_num) :: accum)
                in assemble 0 1 []
end

let%test _ = Day3.max_joltage "10" = 10L
let%test _ = Day3.max_joltage "010" = 10L
let%test _ = Day3.max_joltage "191" = 91L
let%test _ = Day3.max_joltage "987654321111111" = 98L
let%test _ = Day3.max_joltage "811111111111119" = 89L
let%test _ = Day3.max_joltage "234234234234278" = 78L
let%test _ = Day3.max_joltage "818181911112111" = 92L

(* Showing that ultimate_joltage can be used to solve Part 1 as well *)
let%test _ = Day3.ultimate_joltage 2 "10" = 10L
let%test _ = Day3.ultimate_joltage 2 "010" = 10L
let%test _ = Day3.ultimate_joltage 2 "191" = 91L
let%test _ = Day3.ultimate_joltage 2 "987654321111111" = 98L
let%test _ = Day3.ultimate_joltage 2 "811111111111119" = 89L
let%test _ = Day3.ultimate_joltage 2 "234234234234278" = 78L
let%test _ = Day3.ultimate_joltage 2 "818181911112111" = 92L

let%test _ = Day3.ultimate_joltage 12 "100000000000000" = 100000000000L
let%test _ = Day3.ultimate_joltage 12 "111111111111111" = 111111111111L
let%test _ = Day3.ultimate_joltage 12 "123456789123456" = 456789123456L
let%test _ = Day3.ultimate_joltage 12 "987654321111111" = 987654321111L
let%test _ = Day3.ultimate_joltage 12 "811111111111119" = 811111111119L
let%test _ = Day3.ultimate_joltage 12 "234234234234278" = 434234234278L
let%test _ = Day3.ultimate_joltage 12 "818181911112111" = 888911112111L
