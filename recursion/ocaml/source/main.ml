type expr =
  | Number of float
  | Variable of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Neg of expr
  | Power of expr * expr

type token =
  | TNumber of float
  | TVariable of string
  | TPlus | TMinus | TStar | TSlash | THat
  | TLParen | TRParen
  | TEnd

let tokenize str =
  let len = String.length str in
  let rec aux pos acc =
    if pos >= len then List.rev (TEnd :: acc)
    else match str.[pos] with
      | ' ' | '\t' | '\n' -> aux (pos + 1) acc
      | '+' -> aux (pos + 1) (TPlus :: acc)
      | '-' -> aux (pos + 1) (TMinus :: acc)
      | '*' -> aux (pos + 1) (TStar :: acc)
      | '/' -> aux (pos + 1) (TSlash :: acc)
      | '^' -> aux (pos + 1) (THat :: acc)
      | '(' -> aux (pos + 1) (TLParen :: acc)
      | ')' -> aux (pos + 1) (TRParen :: acc)
      | c when c >= '0' && c <= '9' ->
          let start = pos in
          let rec read_num p =
            if p < len && ((str.[p] >= '0' && str.[p] <= '9') || str.[p] = '.')
            then read_num (p + 1) else p in
          let stop = read_num (pos + 1) in
          let num = float_of_string (String.sub str start (stop - start)) in
          aux stop (TNumber num :: acc)
      | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ->
          let start = pos in
          let rec read_var p =
            if p < len && ((str.[p] >= 'a' && str.[p] <= 'z') ||
                          (str.[p] >= 'A' && str.[p] <= 'Z') ||
                          (str.[p] >= '0' && str.[p] <= '9'))
            then read_var (p + 1) else p in
          let stop = read_var (pos + 1) in
          let var = String.sub str start (stop - start) in
          aux stop (TVariable var :: acc)
      | _ -> aux (pos + 1) acc
  in aux 0 []

exception ParseError of string

let rec parse_expression tokens =
  let expr, rest = parse_addition tokens in
  match rest with
  | TEnd :: _ -> expr, rest
  | _ -> expr, rest

and parse_addition tokens =
  let left, rest = parse_multiplication tokens in
  parse_addition_rest left rest

and parse_addition_rest left = function
  | TPlus :: rest ->
      let right, rest' = parse_multiplication rest in
      parse_addition_rest (Add (left, right)) rest'
  | TMinus :: rest ->
      let right, rest' = parse_multiplication rest in
      parse_addition_rest (Sub (left, right)) rest'
  | rest -> left, rest

and parse_multiplication tokens =
  let left, rest = parse_power tokens in
  parse_multiplication_rest left rest

and parse_multiplication_rest left = function
  | TStar :: rest ->
      let right, rest' = parse_power rest in
      parse_multiplication_rest (Mul (left, right)) rest'
  | TSlash :: rest ->
      let right, rest' = parse_power rest in
      parse_multiplication_rest (Div (left, right)) rest'
  | rest -> left, rest

and parse_power tokens =
  let left, rest = parse_factor tokens in
  parse_power_rest left rest

and parse_power_rest left = function
  | THat :: rest ->
      let right, rest' = parse_factor rest in
      Power (left, right), rest'
  | rest -> left, rest

and parse_factor = function
  | TNumber n :: rest -> Number n, rest
  | TVariable v :: rest -> Variable v, rest
  | TMinus :: rest ->
      let expr, rest' = parse_factor rest in
      Neg expr, rest'
  | TLParen :: rest ->
      let expr, rest' = parse_expression rest in
      (match rest' with
       | TRParen :: rest'' -> expr, rest''
       | _ -> raise (ParseError "Missing closing parenthesis"))
  | _ -> raise (ParseError "Unexpected token in factor")

let parse str =
  let tokens = tokenize str in
  let ast, remaining = parse_expression tokens in
  match remaining with
  | [TEnd] -> ast
  | _ -> raise (ParseError "Unexpected tokens at end of expression")

let rec print_tree indent level = function
  | Number n ->
      Printf.printf "%sNumber: %g\n" (String.make (level * 2) ' ') n
  | Variable v ->
      Printf.printf "%sVariable: %s\n" (String.make (level * 2) ' ') v
  | Neg e ->
      Printf.printf "%sNeg:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e
  | Add (e1, e2) ->
      Printf.printf "%sAdd:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e1;
      print_tree indent (level + 1) e2
  | Sub (e1, e2) ->
      Printf.printf "%sSub:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e1;
      print_tree indent (level + 1) e2
  | Mul (e1, e2) ->
      Printf.printf "%sMul:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e1;
      print_tree indent (level + 1) e2
  | Div (e1, e2) ->
      Printf.printf "%sDiv:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e1;
      print_tree indent (level + 1) e2
  | Power (e1, e2) ->
      Printf.printf "%sPower:\n" (String.make (level * 2) ' ');
      print_tree indent (level + 1) e1;
      print_tree indent (level + 1) e2

let rec evaluate = function
  | Number n -> n
  | Variable _ -> failwith "Cannot evaluate variable"
  | Add (e1, e2) -> evaluate e1 +. evaluate e2
  | Sub (e1, e2) -> evaluate e1 -. evaluate e2
  | Mul (e1, e2) -> evaluate e1 *. evaluate e2
  | Div (e1, e2) -> evaluate e1 /. evaluate e2
  | Neg e -> -. (evaluate e)
  | Power (e1, e2) -> (evaluate e1) ** (evaluate e2)

let process_expression expr_str idx =
  Printf.printf "\n=== Выражение %d ===\n" idx;
  Printf.printf "Исходное: %s\n" expr_str;
  Printf.printf "Токены: ";
  List.iter (function
    | TNumber n -> Printf.printf "%.2g " n
    | TVariable v -> Printf.printf "%s " v
    | TPlus -> Printf.printf "+ "
    | TMinus -> Printf.printf "- "
    | TStar -> Printf.printf "* "
    | TSlash -> Printf.printf "/ "
    | THat -> Printf.printf "^ "
    | TLParen -> Printf.printf "( "
    | TRParen -> Printf.printf ") "
    | TEnd -> Printf.printf "END ") (tokenize expr_str);
  Printf.printf "\n";
  try
    let ast = parse expr_str in
    Printf.printf "AST:\n";
    print_tree "" 0 ast;
    try
      let result = evaluate ast in
      Printf.printf "Результат: %g\n" result
    with Failure _ -> Printf.printf "Результат: (содержит переменные)\n"
  with ParseError msg ->
    Printf.printf "Ошибка парсинга: %s\n" msg

let read_input_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let () =
  let input_file = "input/input.txt" in
  try
    let expressions = read_input_file input_file in
    List.iteri (fun idx expr_str ->
      process_expression expr_str (idx + 1)
    ) expressions
  with Sys_error msg ->
    Printf.printf "Ошибка чтения файла %s: %s\n" input_file msg