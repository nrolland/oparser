open MParser
open MParser_RE.Tokens


exception Syntax_error

let infix sym f assoc = Infix  (skip_symbol sym >> return f, assoc)
let prefix sym f = Prefix (skip_symbol sym >> return f)
let negate x = -x
let operators =
[
  [ prefix "-" negate ];
  [ infix "*" ( * ) Assoc_left; infix "/" ( / ) Assoc_left ];
  [ infix "+" ( + ) Assoc_left; infix "-" ( - ) Assoc_left ];
]
let rec term s = (parens expr <|> decimal) s
and expr s = expression operators term s
let eval s =
  match parse_string expr s () with
    | Success x -> x
    | Failed (msg, _) ->
        print_string msg;
        raise Syntax_error

