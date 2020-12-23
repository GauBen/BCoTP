open Bcotp
open Bcotp.Aliases

(* Nouvelle partie *)
let game = Game.new_game ()

;;
Graphics.open_graph " 1280x720";
Graphics.set_window_title "Big Cities on Tiny Planets"

let ui = Userinterface.new_ui game.cities.(0)

let t = ref (time ())

let pt = ref !t

(* Boucle principale du jeu *)

;;
try
  while true do
    t := time ();
    let dt = !t -. !pt in
    pt := !t;

    Graphics.clear_graph ();
    Graphics.auto_synchronize false;

    Game.update game dt;
    Userinterface.update ui;
    Userinterface.draw ui;

    Graphics.synchronize ();
    Unix.sleepf 0.01
  done
with
| Graphics.Graphic_failure _ -> print_string "Bye!\n"
| Exit -> print_string "Bye!\n"