(* values *)
let pi = 3.14159265358979323846264338327950288;;
let half_pi = pi /. 2.;;
let cycle = 2. *. pi;;

(* term of sin(x)'s taylor expansion: (-1)^(k - 1)* x^(2k - 1) / (2k - 1)! *)
let sin_nth_taylor k x =
  let rec fact n =
    if n = 1 then 1
    else n * fact (n - 1)
  in
  let n = k * 2 - 1 in
  if k mod 2 = 1 then
    x ** float_of_int n /. float_of_int (fact n)
  else
    -. x ** float_of_int n /. float_of_int (fact n);;

(* sin(x)'s taylor expansion until (k * 2 - 1) dimention *)
let rec sub_my_sin k x =
  if k = 0 then 0.
  else (sin_nth_taylor k x) +. (sub_my_sin (k - 1) x);;

let primitive_sin = sub_my_sin 7;;

(* fix x into [-pi, pi] *)
let rec into_range x =
  if x > pi then into_range (x -. cycle)
  else if x < -.pi then into_range (x +. cycle)
  else x;;

(* finally!! *)
let my_sin x =
  let fixed_x = into_range x in
  if fixed_x < -.half_pi then
    primitive_sin (-.pi -. fixed_x)
  else if fixed_x > half_pi then
    primitive_sin (pi -. fixed_x)
  else
    primitive_sin fixed_x;;

let my_cos x = my_sin (half_pi -. x);;
