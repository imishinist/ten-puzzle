
let (let*) x k = List.(map k x |> flatten)

(* operators *)
type op = Plus | Minus | Mul | Div

let lift2 f a b =
  match (a, b) with
  | (Some na, Some nb) -> Some (f na nb)
  | _ -> None
let op_func = function
  | Plus -> lift2 ( + )
  | Minus -> lift2 ( - )
  | Mul -> lift2 ( * )
  | Div -> fun a b -> if b = Some 0 then None else lift2 ( / ) a b
let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"

let operators = [Plus; Minus; Mul; Div]

(* expressions *)
type expr = Num of int option | BinOp of op * expr * expr

let expressions a op1 b op2 c op3 d =
  let s a = Num (Some a) in
  BinOp(op3, BinOp (op2, BinOp(op1, s a, s b), s c), s d) ::
  BinOp(op3, BinOp (op1, BinOp(op2, s b, s c), s a), s d) ::
  BinOp(op2, BinOp (op1, s a, s b), BinOp(op3, s c, s d)) ::
  BinOp(op1, BinOp (op3, BinOp(op2, s b, s c), s d), s a) ::
  BinOp(op1, BinOp (op2, BinOp(op3, s c, s d), s b), s a) :: []

let rec string_of_expr = function
  | Num n -> (match n with
              | Some n -> string_of_int n
              | None -> "(none)")
  | BinOp (op, l, r) ->
     Printf.sprintf "(%s %s %s)"
       (string_of_expr l) (string_of_op op) (string_of_expr r)

(* permutation *)
let rec insert x lst =
  match lst with
  | [] -> [[x]]
  | h :: t ->
     (x::lst) :: (List.map (fun el -> h :: el) (insert x t))

let rec perm lst = match lst with
  | [] -> [lst]
  | h :: t ->
     List.flatten (List.map (insert h) (perm t))

let rec eval = function
  | Num n -> n
  | BinOp (op, l,  r) -> op_func (op) (eval l) (eval r)

let solve a b c d =
  let* op1 = operators in
  let* op2 = operators in
  let* op3 = operators in
  expressions a op1 b op2 c op3 d |>
    List.filter (fun e -> (eval e) = Some 10)

let () =
  let (a, b, c, d) = (6, 1, 1, 1) in
  solve a b c d |>
    List.iter (fun e ->
        Printf.printf "%s\n" (string_of_expr e))
