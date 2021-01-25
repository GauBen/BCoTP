(** Aliases *)

let foi = float_of_int

and iof = int_of_float

let len = Array.length

let last arr = len arr - 1

let ord = Char.code

and chr = Char.chr

let sx = Graphics.size_x

and sy = Graphics.size_y

let get arr n = arr.((n mod len arr) + if n < 0 then len arr else 0)

let modpos a b = (a mod b) + if a < 0 then b else 0

let modfl a b =
  (* Modulo sur un flottant *)
  a -. foi (iof a) +. foi (modpos (iof a) b)

let is_between a n b m =
  (* a <= n <= b [mod m] ? *)
  (a <= n && n <= b) || (a - m <= n && n <= b - m) || (a + m <= n && n <= b + m)
