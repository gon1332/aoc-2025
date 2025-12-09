module Day3 = struct
        exception Too_Small_Bank of string

        let max_joltage bank =
                let rec doit bank_list cache =
                        match bank_list with
                        | _::[] -> cache
                        | [] -> raise @@ Too_Small_Bank "The battery bank must include at least two batteries";
                        | num1::rest ->
                                let num2 = List.fold_left max 0 rest in
                                let this_joltage = num1 * 10 + num2 in
                                doit rest @@ if this_joltage > cache then this_joltage else cache
                in
                let bank_list = String.to_seq bank |> List.of_seq |> List.map (fun c -> int_of_char c - int_of_char '0') in
                doit bank_list 0
end

let%test _ = Day3.max_joltage "10" = 10
let%test _ = Day3.max_joltage "010" = 10
let%test _ = Day3.max_joltage "191" = 91
let%test _ = Day3.max_joltage "987654321111111" = 98
let%test _ = Day3.max_joltage "811111111111119" = 89
let%test _ = Day3.max_joltage "234234234234278" = 78
let%test _ = Day3.max_joltage "818181911112111" = 92
