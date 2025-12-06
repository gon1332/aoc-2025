let get_num line =
        int_of_string @@ String.sub line 1 ((String.length line) - 1)

module Day1 = struct
        (* Assumes valid input that starts with 'R' or 'L' *)
        let get_dial line =
                match String.get line 0 with
                | 'R' -> +1 * get_num line
                | _ -> -1 * get_num line

        (* Find the next position and return it. Also return 1 if landed on zero, otherwise 0. *)
        let final_pos current distance =
                let new_pos = current + distance in
                if new_pos >= 0 && new_pos <= 99 then
                        new_pos, if new_pos = 0 then 1 else 0
                else if new_pos > 99 then
                        let final = new_pos mod 100 in
                                final, if final = 0 then 1 else 0
                else (* new_pos < 0 *)
                        let rec see x =
                                if x >= -99 then
                                        (100 + x) mod 100
                                else
                                        see (x + 100)
                        in
                        see new_pos, if new_pos mod 100 = 0 then 1 else 0

        (* Find the next position and return it. Also return the times it visited zero. *)
        let final_pos_2 current distance =
                let new_pos = current + distance in
                if new_pos >= 0 && new_pos <= 99 then
                        new_pos, if new_pos = 0 then 1 else 0
                else if new_pos > 99 then
                        let final = new_pos mod 100 in
                        let times = new_pos / 100 in
                                final, times
                else (* new_pos < 0 *)
                        let rec see x times =
                                if x >= -99 then
                                        let final = (100 + x) mod 100 in
                                                final, times
                                else
                                        see (x + 100) (times + 1)
                        in
                        see new_pos (if current = 0 then 0 else 1)

end

let%test _ = Day1.get_dial "R0" = 0
let%test _ = Day1.get_dial "R1" = 1
let%test _ = Day1.get_dial "R10" = 10
let%test _ = Day1.get_dial "R100" = 100
let%test _ = Day1.get_dial "R1000" = 1000
let%test _ = Day1.get_dial "L0" = 0
let%test _ = Day1.get_dial "L1" = -1
let%test _ = Day1.get_dial "L10" = -10
let%test _ = Day1.get_dial "L100" = -100
let%test _ = Day1.get_dial "L1000" = -1000

let%test _ = Day1.final_pos 50 0 = (50, 0)
let%test _ = Day1.final_pos 50 49 = (99, 0)
let%test _ = Day1.final_pos 50 50 = (0, 1)
let%test _ = Day1.final_pos 50 100 = (50, 0)
let%test _ = Day1.final_pos 50 200 = (50, 0)
let%test _ = Day1.final_pos 50 150 = (0, 1)
let%test _ = Day1.final_pos 50 160 = (10, 0)
let%test _ = Day1.final_pos 50 250 = (0, 1)
let%test _ = Day1.final_pos 50 260 = (10, 0)
let%test _ = Day1.final_pos 50 1060 = (10, 0)
let%test _ = Day1.final_pos 50 (-49) = (1, 0)
let%test _ = Day1.final_pos 50 (-50) = (0, 1)
let%test _ = Day1.final_pos 50 (-60) = (90, 0)
let%test _ = Day1.final_pos 50 (-100) = (50, 0)
let%test _ = Day1.final_pos 50 (-200) = (50, 0)
let%test _ = Day1.final_pos 50 (-150) = (0, 1)
let%test _ = Day1.final_pos 50 (-250) = (0, 1)
let%test _ = Day1.final_pos 50 (-260) = (90, 0)
let%test _ = Day1.final_pos 50 (-760) = (90, 0)
let%test _ = Day1.final_pos 50 (-1060) = (90, 0)

let%test _ = Day1.final_pos_2 50 0 = (50, 0)
let%test _ = Day1.final_pos_2 50 49 = (99, 0)
let%test _ = Day1.final_pos_2 50 50 = (0, 1)
let%test _ = Day1.final_pos_2 50 100 = (50, 1)
let%test _ = Day1.final_pos_2 50 200 = (50, 2)
let%test _ = Day1.final_pos_2 50 150 = (0, 2)
let%test _ = Day1.final_pos_2 50 250 = (0, 3)
let%test _ = Day1.final_pos_2 50 260 = (10, 3)
let%test _ = Day1.final_pos_2 50 1000 = (50, 10)
let%test _ = Day1.final_pos_2 50 (-49) = (1, 0)
let%test _ = Day1.final_pos_2 50 (-50) = (0, 1)
let%test _ = Day1.final_pos_2 50 (-60) = (90, 1)
let%test _ = Day1.final_pos_2 50 (-100) = (50, 1)
let%test _ = Day1.final_pos_2 50 (-200) = (50, 2)
let%test _ = Day1.final_pos_2 50 (-150) = (0, 2)
let%test _ = Day1.final_pos_2 50 (-250) = (0, 3)
let%test _ = Day1.final_pos_2 50 (-260) = (90, 3)
let%test _ = Day1.final_pos_2 50 (-260) = (90, 3)
let%test _ = Day1.final_pos_2 0 (-1) = (99, 0)
let%test _ = Day1.final_pos_2 0 (-10) = (90, 0)
let%test _ = Day1.final_pos_2 0 (-20) = (80, 0)
let%test _ = Day1.final_pos_2 0 (-50) = (50, 0)
let%test _ = Day1.final_pos_2 0 (-100) = (0, 1)
