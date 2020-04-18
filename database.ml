type id = int ;;
let cDBSIZE = 100 ;;
let db : (int, (string * int)) Hashtbl.t =
  Hashtbl.create cDBSIZE ;;

let create (id : id) (name : string) : unit =
  Hashtbl.add db id (name, 0) ;;

let find (id : id) : string * int =
  Hashtbl.find db id

let exists (id : id) : bool =
  try 
    ignore (find id);
    true
  with
  | Not_found -> false ;;

let balance (id : id) : int =
  let _name, bal = find id
  in bal ;;

let name (id : id) : string =
  let name, _bal = find id
  in name ;;

let update (id : id) (value : int) : unit =
  let a = name id in
  Hashtbl.replace db id (a, value) ;;

let close (id : id) : unit =
  Hashtbl.remove db id ;;

let dump () =
  db
  |> Hashtbl.iter (fun i (a, bal) -> Printf.printf "[%d] %s -> %d\n" i a bal) ;;