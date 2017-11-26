(* term of atan(x)'s taylor expansion: (-1)^(k - 1) * x^(2k - 1) / (2k - 1) *)
let atan_nth_taylor k x =
  let n = 2 * k - 1 in
  if k mod 2 = 1 then
    x ** float_of_int n /. float_of_int n
  else
    -. x ** float_of_int n /. float_of_int n;;

(* atan(x)'s taylor expansion until (k * 2 - 1) dimention *)
let rec sub_my_atan k x =
  if k = 0 then 0.
  else (atan_nth_taylor k x) +. (sub_my_atan (k - 1) x);;

let primitive_atan = sub_my_sin 10;;
