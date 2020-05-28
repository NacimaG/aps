
type t = (string * v) list

and v =
    Im of int
  | B of bool 
  | ASTPrim of (v array -> v)
  | FClo of v * (string list) * t
  | RClo of v * (string list)* t * string


let add r x v = (x,v)::r

let adds r xs vs =
  let rec loop xs vs r =
    match xs,vs with
	[], [] -> r
      | (x::xs),(v::vs) -> loop xs vs ((x,v)::r)
      | _,_ -> failwith"Env.adds: should not be: please report"
  in
    loop (List.rev xs) (List.rev vs) r

let extend r xs vs =
  let rec loop xs vs r =
    match xs,vs with
	[], [] -> r
      | ((x,_)::xs),(v::vs) -> loop xs vs ((x,v)::r)
      | _,_ -> failwith"should not be: please report"
  in
    loop (List.rev xs) (List.rev vs) r

let get r x =
  try List.assoc x r
  with Not_found -> failwith ("get: unknown "^x)

  let print r =
    Printf.printf"Env: ";
    List.iter
      (fun (x,v) ->
         match v with
       Im n -> Printf.printf"%s=#%d " x n
     (*| Prim _ -> Printf.printf"%s=[Prim]" x*)
     | FClo _ -> (
         Printf.printf"%s=[Clos]" x
       )
     | RClo _ -> (
         Printf.printf"%s=[RClos]" x
       )
     | _ -> ()    
      )
      r;
  Printf.printf"\n"
