(* #load "str.cma";; *)

exception RPN_Error of string

open Printf

let main text =
	let rec go list stack =
	let pop () = match stack with
		| rhs :: lhs :: tl -> (lhs, rhs, tl)
		| _ -> raise (RPN_Error "too many operators")
	in
	match list with
		| "+" :: next -> let (lhs, rhs, new_stack) = pop () in
			go next ([lhs +. rhs] @ new_stack)
		| "-" :: next -> let (lhs, rhs, new_stack) = pop () in
			go next ([lhs -. rhs] @ new_stack)
		| "*" :: next -> let (lhs, rhs, new_stack) = pop () in
			go next ([lhs *. rhs] @ new_stack)
		| "/" :: next -> let (lhs, rhs, new_stack) = pop () in
			go next ([lhs /. rhs] @ new_stack)
		| "^" :: next -> let (lhs, rhs, new_stack) = pop () in
			go next ([lhs ** rhs] @ new_stack)
		| "" :: next -> go next stack
		| hd :: next -> go next ([float_of_string hd] @ stack)
		| [] -> stack
	in
	let r = Str.regexp "[ \t]" in
	let l = Str.split r text in
	let res () = match (go l []) with
		| [] -> raise (RPN_Error "no expression made")
		| [item] -> item
		| hd :: tl -> raise (RPN_Error "not enough operators")
	in
	try
		res ()
	with
		Failure msg -> raise(RPN_Error "value was not a number or valid operator")

let() = while true do
	try
		printf "%f\n" (main (read_line ()))
	with
		RPN_Error msg -> printf "%s\n" msg
done

