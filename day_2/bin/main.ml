open Day2

let read_channel ic strategy =
        try
                let act range = Day2.range_from_str range |> strategy in
                input_line ic |> String.split_on_char ',' |> List.map act |> List.fold_left ( + ) 0
        with e ->
                match e with
                | End_of_file -> 0
                | _ -> raise e

let () =
        if Array.length Sys.argv != 2 then
                print_endline "Provide the path to the input as an argument"
        else
                let file = Array.get Sys.argv 1 in
                let ic = open_in file in
                let finally () = close_in ic in
                let work () =
                        Printf.printf "Sum of invalid IDs %i\n" @@ read_channel ic Day2.sum_of_invalid_ids;
                        seek_in ic 0;
                        Printf.printf "Sum of correct invalid IDs %i\n" @@ read_channel ic Day2.sum_of_invalid_ids_2;
                in
                Fun.protect ~finally work;
