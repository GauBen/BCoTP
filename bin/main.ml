open Bcotp
open Aliases

let () =
  Graphics.open_graph " 1280x720";
  Graphics.set_window_title "Big Cities on Tiny Planets"

let new_seed () =
  let seed = iof (time ()) in
  print_endline ("Creating a new world with seed " ^ string_of_int seed);
  seed

let seed =
  ref
    (if len Sys.argv = 2 then
     try int_of_string Sys.argv.(1) with Failure _ -> new_seed ()
    else new_seed ())

let () =
  try
    while true do
      try
        (* Nouvelle partie *)
        let game = Game.new_game !seed in

        let ui = Userinterface.new_ui game.cities.(0) in

        let t = ref (time ()) in
        let pt = ref !t in

        (* Boucle principale du jeu *)
        while true do
          t := time ();
          let dt = !t -. !pt in
          pt := !t;

          Graphics.clear_graph ();
          Graphics.auto_synchronize false;

          Game.update game dt;
          Userinterface.update ui dt;
          Userinterface.draw ui;

          Graphics.synchronize ();
          Unix.sleepf 0.016
        done
      with
      | Replay -> ()
      | New -> seed := new_seed ()
    done
  with
  | Graphics.Graphic_failure _ -> print_string "Bye!\n"
  | Exit -> print_string "Bye!\n"
