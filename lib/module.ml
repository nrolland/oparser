open MParser

let infix p op = Infix (p |>> (fun _ a b -> (`Binop (op, a, b))), Assoc_left)

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

let decimal = many1_chars digit |>> int_of_string

let expr = expression operators (decimal |>> fun i -> `Int i)

let rec calc = function
  | `Int i -> i
  | `Binop (op, a, b) ->
      match op with
        | `Add -> calc a + calc b
        | `Sub -> calc a - calc b
        | `Mul -> calc a * calc b
        | `Div -> calc a / calc b
