open StdLabels
open MoreLabels

type 'a t =
  | Left of 'a
  | Equal of 'a
  | Right of 'a

let lcs x y =
  let m = Array.length x
  and n = Array.length y in
  let c = Array.create_matrix ~dimx:(m + 1) ~dimy:(n +1) 0 in
  for i = 1 to m do
    for j = 1 to n do
        c.(i).(j) <- if x.(i-1) = y.(j-1) then
          c.(i-1).(j-1) + 1
        else
          max c.(i).(j-1) c.(i-1).(j)
    done
  done;
  c

let rec diff x y =
  let c = lcs x y in
  let rec loop acc i j =
    if i > 0 && j > 0 && x.(i-1) = y.(j-1) then
      loop (Equal x.(i-1) :: acc) (i-1) (j-1)
    else if j > 0 && (i = 0 || c.(i).(j-1) >= c.(i-1).(j)) then
      loop (Right y.(j-1)::acc) i (j-1)
    else if i > 0 && (j = 0 || c.(i).(j-1) < c.(i-1).(j)) then
      loop (Left x.(i-1) :: acc) (i-1) j
    else
      acc
  in
  loop [] (Array.length x) (Array.length y)


let print =
  List.iter ~f:(function
                  | Equal s -> print_string " "; print_endline s
                  | Left s -> print_string "-"; print_endline s
                  | Right s -> print_string "+"; print_endline s
               )

let explode s =
  let len = String.length s in
  let res = Array.create len "" in
  for i = 0 to len - 1 do
    let s' = String.create 1 in
    s'.[0] <- s.[i];
    res.(i) <- s'
  done;
  res

let test x y =
  let x = explode x
  and y = explode y in
  print (diff x y)


let _ = test "chimpanzee" "human"
