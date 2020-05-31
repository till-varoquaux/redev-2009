type custom = exn -> string option

type customs = {
  mutable len : int;
  mutable lst : custom Weak.t;
}

let customs = {
  len = 0;
  lst = Weak.create 16
}

let compact () =
  let size = ref 0 in
  for idx = 0 to Weak.length customs.lst - 1 do
    if Weak.check customs.lst idx then
      incr size
  done;
  Printf.eprintf "compacting %i\n%!" !size;
  let lst = Weak.create (2* !size + 2) in
  let len = ref 0 in
  for idx = 0 to Weak.length customs.lst - 1 do
    match Weak.get customs.lst idx with
    | None -> ()
    | Some _ as v ->
        Weak.set lst !len v;
        incr len
  done;
  customs.lst <- lst;
  customs.len <- !len

let add_custom pr =
  if customs.len = Weak.length customs.lst - 1 then
    compact ();
  Weak.set customs.lst customs.len (Some pr);
  customs.len <- customs.len + 1


let to_string exc =
  let rec aux pos =
    if pos < 0 then
      Printexc.to_string exc
    else
      match Weak.get customs.lst pos with
      | None -> aux (pos  - 1)
      | Some f ->
          match f exc with
          | Some v -> v
          | None -> aux (pos - 1)
  in
  aux (customs.len - 1)

let cnt = ref 0
let cust () =
  incr cnt;
  add_custom (function
                | Not_found -> Some (string_of_int !cnt)
                | _ -> None)
let _ = for i = 0 to 20 do
  cust ()
done

let _ = !cnt

let 
let _ = Gc.full_major ()
let _ = compact ()
let _ = customs
