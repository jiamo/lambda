open Angstrom
open Printf

type term =
  | Var of string
  | App of term * term
  | Lam of string * term

let rec show_term = function
  | Lam (s, t) ->
    let rec showB = function
      | Lam (x, y) -> " " ^ x ^ showB y
      | expr -> "." ^ show_term expr
    in
    "λ" ^ s ^ showB t
  | Var s -> s
  | App (x, y) ->
    let showL = function
      | Lam (_, _) -> "(" ^ show_term x ^ ")"
      | _ -> show_term x
    and showR = function
      | Var s -> " " ^ s
      | _ -> "(" ^ show_term y ^ ")"
    in
    showL x ^ showR y

type lambda_line =
  | Blank
  | Let of string * term
  | Run of term

let rec eval env = function
  | App (Var "quote", t) -> quote env t
  | App (m, a) ->
    (match eval env m with
     | Lam (v, f) -> eval env (beta env (v, a) f)
     | _ -> App (m, a))
  | Var v ->
    (match List.assoc_opt v env with
     | Some x -> eval env x
     | None -> Var v)
  | term -> term


and beta env (v, a) t =
  let beta' = beta env (v, a) in
  let fvs = fv env [] a in
  match t with
  | Var s when s = v -> a
  | Var s -> Var s
  | Lam (s, m) when s = v -> Lam (s, m)
  | Lam (s, m) ->
    if List.mem s fvs then
      let s1 = newName s (v :: (fv env [] a @ fv env [] m)) in
      Lam (s1, beta' (rename s s1 m))
    else
      Lam (s, beta' m)
  | App (m, n) -> App (beta' m, beta' n)


and fv env vs = function
  | Var s when List.mem s vs -> []
  | Var s ->
    (match List.assoc_opt s env with
     | Some x -> fv env (s :: vs) x
     | None -> [s])
  | App (x, y) -> fv env vs x @ fv env vs y
  | Lam (s, f) -> fv env (s :: vs) f

and newName x ys =
  let base = match Str.split (Str.regexp "_") x with
    | [] -> ""
    | hd :: _ -> hd
  in
  let rec aux i =
    let name = base ^ "_" ^ string_of_int i in
    if List.mem name ys then aux (i + 1) else name
  in
  aux 1

and rename x x1 term =
  let rec aux = function
    | Var s when s = x -> Var x1
    | Var s -> Var s
    | Lam (s, b) when s = x -> Lam (s, b)
    | Lam (s, b) -> Lam (s, aux b)
    | App (a, b) -> App (aux a, aux b)
  in
  aux term

and quote env term =
    let fvs = fv env [] term in
    let abc = List.map (fun s -> if List.mem s fvs then newName s fvs else s) ["a"; "b"; "c"] in
    let a, b, c = 
      match abc with
      | [a; b; c] -> a, b, c
      | _ -> failwith "Expected a list with three elements"in
    let f n g = Lam (a, Lam (b, Lam (c, g (Var (List.nth abc n))))) in
    let rec aux = function
      | Var x ->
        (match List.assoc_opt x env with
         | Some t -> aux t
         | None -> f 0 (fun v -> App (v, Var x)))
      | App (m, n) -> f 1 (fun v -> App (App (v, aux m), aux n))
      | Lam (x, m) -> f 2 (fun v -> App (v, Lam (x, aux m)))
    in
    aux term
  
let rec norm env term =
  match eval env term with
  | Var v -> Var v
  | Lam (v, m) -> Lam (v, norm env m)
  | App (m, n) -> App (norm env m, norm env n)

let ws = skip_while (function ' ' | '\t' -> true | _ -> false)
let str s = string s <* ws
let v =
  print_endline "Parsing variable";
  let result = take_while1 (function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false) <* ws in
  print_endline "Finished parsing variable";
  result
let between left right p =
  let* _ = left in
  let* x = p in
  let* _ = right in
  return x
  
let term_parser term =
  choice [
    (let* _ = choice [ str "\\"; str "λ" ] in
      let* vars = many1 v in
      let* _ = choice [ str "->"; str "." ] in
      let* t = term in
      return (List.fold_right (fun var acc -> Lam (var, acc)) vars t));
    (let* terms = many1 (choice [ (v >>| fun s -> Var s); between (str "(") (str ")") term ]) in
      match terms with
      | [] -> fail "unexpected"
      | hd :: tl -> return (List.fold_left (fun acc t -> App (acc, t)) hd tl))
  ]

let term = fix term_parser
let line_parser =
  print_endline "Starting parsing";
  let result = ws *> (option Blank (choice [
    (print_endline "Trying Let parser";
      let* var = v in
      let* _ = str "=" in
      let* t = term  in
      print_endline "Finished Let parser";
      return (Let (var, t)));
    (print_endline "Trying Run parser";
      term >>| fun t -> 
      print_endline "Finished Run parser";
      Run t)
  ])) <* end_of_input in
  print_endline "Finished parsing";
  result

let rec repl env =
  print_string "> ";
  let s = read_line () in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All line_parser s with
  | Ok Blank -> repl env
  | Ok (Run term) ->
    printf "%s\n" (show_term (norm env term));
    repl env
  | Ok (Let (v, term)) -> repl ((v, term) :: env)
  | Error msg ->
    printf "parse error: %s\n" msg;
    repl env
let main () = repl []
let () = main ()