
let (let+) x f = Option.bind x f
let (let*) x f = List.(map f x |> flatten)

(* rational *)
(* first / second *)
module Rational = struct
  type t = int * int

  exception GcdNegativeException
  let rec gcd a b =
    if a < 0 || b < 0 then raise GcdNegativeException
    else match (a, b) with
      | (a, 0) -> a
      | (0, b) -> b
      | _ -> if a > b then gcd b (a mod b) else gcd a (b mod a)

  let rec init a b =
    if b < 0 then init (-a) (-b)
    else
      let (m, n, is_negative) = if a < 0 then (-a, b, true) else (a, b, false) in
      let d = gcd m n in
      let (p, q) = (m / d, n / d) in
      if is_negative then (-p, q) else (p, q)

  let ( /+ ) (p, q) (r, s) = init (p * s + r * q) (q * s)
  let ( /- ) (p, q) (r, s) = init (p * s - r * q) (q * s)
  let ( /* ) (p, q) (r, s) = init (p * r) (q * s)
  let ( // ) a (r, s) = a /* (s, r)
  let ( /= ) (p, q) (r, s) =
    let (p', q') = init p q in
    let (r', s') = init r s in
    p' = r' && q' = s'

  let numer (a, _) = a
  let denom (_, b) = b

  let string_of_rational (a, b) =
    if b == 1 then string_of_int a
    else string_of_int a ^ "/" ^ string_of_int b
end

(* operators *)
type op = Plus | Minus | Mul | Div

let lift2 f a b =
  match (a, b) with
  | (Some na, Some nb) -> Some (f na nb)
  | _ -> None
let op_func = function
  | Plus -> lift2 ( Rational.(/+) )
  | Minus -> lift2 ( Rational.(/-) )
  | Mul -> lift2 ( Rational.(/*) )
  | Div -> fun a b ->
    let+ (p, _) = b in
    if p = 0 then None
    else lift2 ( Rational.(//) ) a b
let string_of_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"

let operators = [Plus; Minus; Mul; Div]

(* expressions *)
type expr = Num of Rational.t option | BinOp of op * expr * expr

let expressions a op1 b op2 c op3 d =
  let s a = Num (Some a) in
  BinOp(op3, BinOp (op2, BinOp(op1, s a, s b), s c), s d) ::
  BinOp(op3, BinOp (op1, s a, BinOp(op2, s b, s c)), s d) ::
  BinOp(op2, BinOp (op1, s a, s b), BinOp(op3, s c, s d)) ::
  BinOp(op1, s a, BinOp (op3, BinOp(op2, s b, s c), s d)) ::
  BinOp(op1, s a, BinOp (op2, s b, BinOp(op3, s c, s d))) :: []

let rec string_of_expr = function
  | Num n -> (match n with
              | Some n -> Rational.string_of_rational n
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

let rec print_answers = function
  | [] -> ()
  | h :: t ->
     Printf.printf "%s\n" (string_of_expr h);
     print_answers t

let extract lst =
  let a = List.hd lst in
  let lst = List.tl lst in
  let b = List.hd lst in
  let lst = List.tl lst in
  let c = List.hd lst in
  let lst = List.tl lst in
  let d = List.hd lst in
  (a, b, c, d)

let solve lst =
  let lst = List.map (fun e -> Rational.init e 1) lst in
  let (a, b, c, d) = extract lst in
  let* op1 = operators in
  let* op2 = operators in
  let* op3 = operators in
  expressions a op1 b op2 c op3 d |>
    List.filter (fun e -> match (eval e) with
      | None -> false
      | Some n -> Rational.(/=) n (10, 1))

let () =
  let input = Scanf.sscanf (read_line ()) "%d %d %d %d" (fun a b c d -> [a; b; c; d]) in
  let ans1 = solve input in
  let ans2 = lazy (perm input |>
               List.filter_map (fun e -> match solve e with
                 | [] -> None
                 | ans -> Some ans) |>
               List.flatten) in
  match ans1 with
  | [] -> print_answers (Lazy.force ans2)
  | _ -> print_answers ans1