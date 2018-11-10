open MParser

let infix p op = Infix (p |>> (fun _ a b -> (`Binop (op, a, b))), Assoc_left)

let decimal = many1_chars digit |>> int_of_string
let term = (decimal |>> fun i -> `Int i)

(* type annotation necessary if expr not used*)
let operators : ([ `Binop of [ `Add | `Div | `Mul | `Sub ] * 'a * 'a | `Int of int ] as 'a, unit) operator list list  =
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
let expr s  =  expression operators term s

let rec calc = function
  | `Int i -> i
  | `Binop (op, a, b) ->
      match op with
        | `Add -> calc a + calc b
        | `Sub -> calc a - calc b
        | `Mul -> calc a * calc b
        | `Div -> calc a / calc b


let eval (s: string) : int =
  match MParser.parse_string expr s () with
    | Success e ->
        calc e
    | Failed (msg, _) ->
        failwith msg
