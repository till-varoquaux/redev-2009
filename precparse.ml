type dir = Left | Right

type 'expr t =
  | Const of 'expr
  | Unary of int * ('expr -> 'expr)
  | Bin of int * dir * ('expr -> 'expr -> 'expr)
  | Opar
  | Cpar
  | End

type 'expr state = {
  mutable tok : 'expr t;
  lex : unit -> 'expr t;

  mutable next : 'expr t option;
  default_bin : 'expr t
}

let shift t =
  match t.next with
  | Some n ->
      t.next <- None;
      t.tok <- n
  | None ->
      if t.tok <> End then
        match t.tok,t.lex () with
        | (Const _ | Cpar | Unary _ ), ((Const _ | Opar ) as v) ->
            t.tok <- t.default_bin;
            t.next <- Some v
        | _,v -> t.tok <- v

let expect t tok =
  if t.tok <> tok then
    failwith "parse error";
  shift t

let rec exp src lvl =
  exp' (p src) src lvl
and exp' left src lvl =
  match src.tok with
  | Bin (prec,dir,cons) when prec >= lvl ->
      consume src;
      let prec = match dir with
        | Right -> prec
        | Left -> prec + 1
      in
      exp' (cons left (exp src prec)) src lvl
  | _ -> left
and p src =
  match src.tok with
  | Unary (prec,cons) ->
      shift src;
      let t = exp src prec in
      cons t
  | Opar ->
      shift src;
      let r = exp src 0 in
      expect src Cpar;
      r
  | Const c ->
      shift src;
      c
  | _ -> assert false

let parse default_bin lex =
  let src = {
    tok = lex ();
    next = None;
    lex = lex;
    default_bin = default_bin
  }
  in
  let res = exp src 0 in
  expect src End;
  res

type string_lex = {
  mutable pos : int;
  len : int;
  str : string;
}

let get t =
  if t.pos = t.len  then
    End
  else
    let res = match t.str.[t.pos] with
      | '*' -> Unary (8,fun x -> Star x)
      | '|' -> Bin (7,Right,fun x y -> Or (x,y))
      | '&' -> Bin (7,Right,fun x y -> And (x,y))
      | '!' -> Unary (6,fun x -> Not x)
      | '(' -> Opar
      | ')' -> Cpar
      | c -> Const (Char c)
    in
    t.pos <- t.pos + 1;
    res

let of_string s =
  let lex_src = {
    pos = 0;
    len = String.length s;
    str = s
  }
  in
  parse (Bin (9,Right,fun x y -> Cons (x,y))) (fun () -> get lex_src)

let _ = of_string "abcd|def"
