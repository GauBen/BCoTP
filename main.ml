(* Bibliothèques natives pour l'affichage 2D et la gestion du temps *)
#load "graphics.cma";;
#load "unix.cma";;

(* Aliases *)
let foi = float_of_int and iof = int_of_float;;
let len = Array.length;;
let last arr = len arr - 1;;
let ord = Char.code and chr = Char.chr;;
let sx = Graphics.size_x and sy = Graphics.size_y;;
let time = Sys.time;;
let get arr n = arr.(n mod len arr + if n < 0 then len arr else 0);;
let modpos a b = a mod b + (if a < 0 then b else 0);;
let modfl a b = (* Modulo sur un flottant *)
	a -. foi (iof a) +. foi (modpos (iof a) b);;
let is_between a n b m = (* a <= n <= b [mod m] ? *)
	(a <= n && n <= b)
	|| (a - m <= n && n <= b - m)
	|| (a + m <= n && n <= b + m);;

(* Chargement des ressources *)
#use "./resources.ml";;
#use "./game.ml";;
#use "./ui.ml";;

(* Nouvelle partie *)
let game = Game.new_game ();;

Graphics.open_graph "1280x720";
Graphics.set_window_title "Big Cities on Tiny Planets";;

let ui = UserInterface.new_ui (game.cities.(0))

let t = ref (time ())
let pt = ref !t;;

(* Boucle principale du jeu *)
try while true do

	t := time ();
	let dt = !t -. !pt in
	pt := !t;

	Graphics.clear_graph ();
	Graphics.auto_synchronize false;

	Game.update game dt;
	UserInterface.update ui;
	UserInterface.draw ui;

	Graphics.synchronize ();
	Unix.sleepf 0.01;

done;
with
	| Graphics.Graphic_failure e -> print_string "Bye!\n"
	| Exit -> print_string "Bye!\n";;
