(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cache.mli,v 1.3 2004/09/24 10:55:05 weis Exp $ *)

type ('a, 'b) elt = { key : 'a; data : 'b; time : float; };;
type ('a, 'b) t = ('a, 'b) elt option array;;

val create : int -> ('a, 'b) t;;
val find_pos : ('a, 'b) t -> 'a -> int;;
val find : ('a, 'b) t -> 'a -> 'b;;
val rename : ('a, 'b) t -> 'a -> 'a -> unit;;
val find_empty_or_eldest : ('a, 'b) t -> int;;
val add : ('a, 'b) t -> 'a -> 'b -> unit;;
