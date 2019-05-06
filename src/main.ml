open Lexing
open Parser
open Lexer
open Poly
open Expr

let filename = if Array.length Sys.argv >= 2
	then Sys.argv.(1)
	else failwith "missing filename as arg"

let () = 
	open_in filename |>
	Lexing.from_channel |>
	Parser.main Lexer.token |>
	print_expr |>
	from_expr |>
	simplify |>
	print_pExp
