
open MParser

(* type nonrec ('a, 's) operator =
 *     Infix of (('a -> 'a -> 'a, 's) t * assoc)
 *   | Prefix of ('a -> 'a, 's) t
 *   | Postfix of ('a -> 'a, 's) t *)

(* infix :  ('a, 'b) t -> 'c -> ([> `Binop of 'c * 'd * 'd ] as 'd, 'b) operator*)
let infix p op =
  Infix (p |>> (fun _ a b -> (`Binop (op, a, b))), Assoc_left)


(* operators :
  (_[> `Binop of _[> `Add | `Div | `Mul | `Sub ] * 'a * 'a ] as 'a, '_weak1)
  operator list list =
  [[Infix (<fun>, Assoc_left); Infix (<fun>, Assoc_left)];
   [Infix (<fun>, Assoc_left); Infix (<fun>, Assoc_left)]] *)
let operators =
  [
    [
      infix (char '*') `Mul;
      infix (char '/') `Div;
    ];
    [
      infix (char '+') `Add;
      infix (char '-') `Sub;
    ];
  ]

let decimal =
  many1_chars digit |>> int_of_string

let expr =
  expression operators (decimal |>> fun i -> `Int i)

let rec calc = function
  | `Int i -> i
  | `Binop (op, a, b) ->
      match op with
        | `Add -> calc a + calc b
        | `Sub -> calc a - calc b
        | `Mul -> calc a * calc b
        | `Div -> calc a / calc b


