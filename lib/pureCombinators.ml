
type token = LPAR | RPAR | COMMA | DOT | SEMIC | COLON
           | LBRACK | RBRACK | LBRACE | RBRACE
	         | EQ | LT | LE | GT | GE
	         | PLUS | MINUS | MULT | DIV
	         | STAR | DOLLAR | SHARP | UNDERSCORE | CARET | BAR
	         | IF | THEN | ELSE | FUN | ARROW
	         | LET | REC | IN
           | INT of int
	         | BOOL of bool
	         | CONSTR0 of string
	         | CONSTRN of string
	         | IDENT of string


type 'a btree = Leaf of 'a | Bin of 'a btree * 'a btree;;


let ts = "(2,(3,4))"
let tk = [LPAR; INT 2; COMMA; LPAR; INT 3; COMMA; INT 4; RPAR; RPAR]
let t = Bin(Leaf 2, Bin(Leaf 3, Leaf 4))


type ('a,'t) parser = 't list -> 'a * 't list

exception SyntaxError

let ($) a : ('t, 't) parser = function (h:: toks) -> if a=h then (a,toks) else raise SyntaxError  | _ -> raise SyntaxError
let int : (int,token) parser = function (INT i:: toks) -> (i,toks) | _ -> raise SyntaxError

let empty toks = ([],toks)
let (||) p q toks = try p toks with _ -> q toks
let (!!) p toks = try p toks with _ -> failwith "bad format"
let (--) p q toks = let (x,t2) = p toks in let (y,t3) = q t2 in ((x,y), t3)
let (>>) p f toks = let (x,t) = p toks in (f x, t)

let ($--) (k:'t) (q:('a,'t) parser) : ('a,'t) parser =  (($) k --  !! q) >> (fun (_,x) -> x)
let (--$) (p:('a,'t) parser) (k:'t) : ('a,'t) parser = ((!! p) -- ($) k) >> (fun (x,_) -> x)

let rec repeat p toks =
  let p2 = (p -- repeat p ) in
  let p3 = p2 >> (fun (x,xs) -> x::xs) in
  (p3 || empty) toks


let between k k' p = k $-- ( p --$ k')
let between2 k k' s p = k $-- ( (p --$ s) -- p --$ k')


let ex = ($) EQ
let  _ = ex || ex


let x () : (int btree, token) parser = failwith ""


let rec pleaf : (int btree, token) parser = (int >> (fun i -> Leaf i))
and pbin  : (int btree, token) parser = fun t -> ((between2 LPAR RPAR COMMA ptree) >> (fun (l,r) -> Bin(l,r))) t
and ptree : (int btree, token) parser = fun t -> (pleaf || pbin) t



let xi = int [ INT 15]
let xl = pleaf [ INT 15]
let xtree = ptree tk

let lex  () : token list = failwith ""

let btree () : int btree = failwith ""
