open Day3

let read_channel ic strategy =
        let rec accumulate total_joltage =
                try
                        input_line ic |> strategy |> (+) total_joltage |> accumulate
                with e ->
                        match e with
                        | End_of_file -> total_joltage
                        | _ -> raise e
        in accumulate 0

let () =
        if Array.length Sys.argv != 2 then
                print_endline "Provide the path to the input as an argument"
        else
                let file = Array.get Sys.argv 1 in
                let ic = open_in file in
                let finally () = close_in ic in
                let work () =
                        Printf.printf "Total output joltage is %i\n" @@ read_channel ic Day3.max_joltage;
                        seek_in ic 0;
                in
                Fun.protect ~finally work;
