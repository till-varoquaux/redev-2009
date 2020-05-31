type t =
  | Fail
  | Nil
  | Cons of t*t
  | Char of char
  | Star of t
  | Or of t*t
  | And of t*t
  | Not of t

let (&&&) x y =
  match x,y with
  | Fail,_ -> Fail
  | _,Fail -> Fail
  | _ when x = y -> x
  | _ -> And (x,y)

let (|||) x y = match x,y with
  | Fail,_ -> y
  | _,Fail -> x
  | _ when x = y -> x
  | _ -> Or (x,y)

let cons x y = match x,y with
  | Nil,_ -> y
  | _,Nil -> x
  | _ -> Cons (x,y)

let (!!) =  function
  | Not x -> x
  | x -> Not x

let rec is_nullable = function
  | Fail -> false
  | Nil -> true
  | Cons (t,t') -> is_nullable t && is_nullable t'
  | Char _ -> false
  | Or (t,t') -> is_nullable t ||  is_nullable t'
  | And (t,t') -> is_nullable t && is_nullable t'
  | Not t -> not (is_nullable t)
  | Star _ -> true

let rec deriv_chr chr = function
  | Char chr' when chr = chr' -> Nil
  | Char _ -> Fail
  | Fail | Nil  -> Fail
  | Cons (x,y) when is_nullable x ->
      (cons (deriv_chr chr x) y) ||| (deriv_chr chr y)
  | Cons (x,y) ->
      (cons (deriv_chr chr x) y)
        (* TODO: implement some laziness *)
  | And (x,y) ->
      (deriv_chr chr x) &&& (deriv_chr chr y)
  | Or (x,y) -> (deriv_chr chr x) ||| (deriv_chr chr y)
  | Not x -> !!(deriv_chr chr x)
  | Star x as v -> cons (deriv_chr chr x) v
