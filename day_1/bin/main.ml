open Day1

(* Calculates the new position and the times we visited zero according to the chosen strategy *)
let calculate strategy prev_pos dial =
        if dial = 0 then
                prev_pos, 0
        else
                strategy prev_pos dial

(* Iterates through a file to calculate the code according to the chosen strategy *)
let rec read_channel ic prev_pos times strategy =
        try
                let line = input_line ic in
                let dial = Day1.get_dial line in
                let new_pos, times_visited_zero = calculate strategy prev_pos dial in
                        read_channel ic new_pos (times + times_visited_zero) strategy
        with e ->
                match e with
                | End_of_file -> times
                | _ -> raise e

let () =
        if Array.length Sys.argv != 2 then
                print_endline "Provide the path to the input as an argument"
        else
                let file = Array.get Sys.argv 1 in
                let ic = open_in file in
                let finally () = close_in ic in
                let work () =
                        Printf.printf "Old code is %i\n" @@ read_channel ic 50 0 Day1.final_pos;
                        seek_in ic 0;
                        Printf.printf "Actual code is %i\n" @@ read_channel ic 50 0 Day1.final_pos_2;
                in
                Fun.protect ~finally work;
