type id = int ;;

val create : id -> string -> unit ;;
val exists : id -> bool ;;
val balance : id -> int ;;
val name : id -> string ;;
val update : id -> int -> unit ;;
val close : id -> unit ;;
val dump : unit -> unit ;;