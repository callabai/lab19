open Printf ;;
open Scanf ;;
module DB = Database ;;

type id = int ;;

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)
let initialize (initial : account_spec list) : unit =
  initial
  |> List.iter (fun {name; id; balance}
                -> DB.create id name;
                   DB.update id balance) ;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let rec acquire_id () : int =
  printf "Enter customer id: ";
  try
    let id = read_int () in 
    ignore (DB.exists id); id 
  with
  | Not_found
  | Failure _ -> printf "Invalid id \n";
                 acquire_id () ;;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let rec acquire_amount () : int =
  printf "Enter amount: ";
  try
    let amount = read_int () in
    if amount <= 0 then raise (Failure "amount is negative");
    amount
  with
  | Failure _ -> printf "Invalid amount \n";
                 acquire_amount () ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
let rec acquire_act () : action =
  printf "Enter action: (B) Balance (-) Withdraw (+) Deposit \ 
           (=) Done (X) Exit: %!";
  scanf " %c"
        (fun char -> match char with
                     | 'b' | 'B'        -> Balance
                     | '/' | 'x' | 'X'  -> Finished
                     | '='              -> Next
                     | 'w' | 'W' | '-'  -> Withdraw (acquire_amount ())
                     | 'd' | 'D' | '+'  -> Deposit (acquire_amount ())
                     | _                -> printf "  invalid choice\n";
                                           acquire_act () ) ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id. 
 *)
  
(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
let get_balance : id -> int = DB.balance ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name : id -> string = DB.name ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id,setting it to the given amount. *)
let update_balance : id -> int -> unit = DB.update ;;

(*....................................................................
  Presenting information and cash to the customer
 *)
  
(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
let present_message (msg: string) : unit =
  printf "%s\n%!" msg ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash (amount : int) : unit =
  printf "Here's your cash: ";
  for _i = 1 to (amount/20) do 
    printf "[20 @ 20]"
  done;
  printf " and %d more \n" (amount mod 20) ;;