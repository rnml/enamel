(*
   Copyright (c) 1999 Christian Lindig <lindig@ips.cs.tu-bs.de>. All
   rights reserved. See COPYING for details.

   Modified by Nathan Mishra Linger (2006)
*)

open Core.Std

(* debugging makes groups visible *)
let debug = false

(* Auxiliary functions *)
let strlen = String.length

(* definitions *)
let nl = "\n"

(* A type for the different kind of (user visible) groups *)
type gmode =
  | GFlat  (* hgrp *)
  | GBreak (* vgrp *)
  | GFill  (* fgrp *)
  | GAuto  (* agrp *)

(* users build (complex) documents of this type *)
type t =
  | DNil
  | DCons of t * t
  | DText of string
  | DNest of int * t
  | DBreak of string
  | DGroup of gmode * t

(*  constructor functions for documents *)

let (^^) x y    = DCons (x, y)
let empty       = DNil
let text s      = DText s
let nest i x    = DNest (i, x)
let break       = DBreak " "
let break_with s = DBreak s

let hgrp d      = DGroup (GFlat, d)
let vgrp d      = DGroup (GBreak, d)

let agrp d =
  if debug
  then DGroup (GAuto, text "[" ^^ d ^^ text "]")
  else DGroup (GAuto, d)

let fgrp d =
  if debug
  then DGroup (GFill, text "{" ^^ d ^^ text "}")
  else DGroup (GFill, d)

(*
   Formatting turns a complex [doc] document into a simpler
   [sdoc] document -- just a linked list of strings with
   the newline strings treated specially.

   [sdoc]s are lazily formatted, so even enormous documents can still be
   formatted incrementally.  The increments are lines: all chars on a
   line will be produced and the remainder thunked.
*)
type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * (unit -> sdoc) (* newline + spaces *)

(* [sdoc_to_string] formats a simple document into a string: [SLine]
   line breaks are expanded into a newline followed by spaces *)

(* uses the String_cat library for efficient concatenation
   of a potentially long list of strings. *)
let sdoc_to_string d =
  let (+) xs x = String_pad.add xs x in
  let rec loop acc = function
    | SNil -> acc
    | SText(s, d) -> loop (acc + s) d
    | SLine(i, d) ->
      let prefix = String.make i ' ' in
      loop ((acc + nl) + prefix) (d ())
  in
  String_pad.dump (loop String_pad.empty d)

(* [sdoc_to_file oc doc] formats [doc] into output channel [oc] *)

let sdoc_to_file oc doc =
  let pstr = output_string oc in
  let rec loop = function
    | SNil -> ()
    | SText(s, d) -> pstr s; loop d
    | SLine(i, d) ->
      let prefix = String.make i ' ' in
      pstr nl;
      pstr prefix;
      loop (d ())
  in
  loop doc

(* [agrp]s are turned into [Flat] or [Break] groups - so their are
   only 3 different modes internally. *)

type mode = Flat | Break | Fill

(* [fits w] checks whether a document up to the next [break] fits into [w]
   characters. All kind of groups are considered flat: their breaks count
   as spaces. This means the [break] this function looks for must not be
   inside sub-groups *)

let rec fits w = function
  | _ when w < 0                 -> false
  | []                           -> true
  | (_, _, DNil)          :: z -> fits w z
  | (i, m, DCons(x, y))   :: z -> fits w ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DNest(j, x))   :: z -> fits w ((i + j, m, x) :: z)
  | (_, _, DText(s))      :: z -> fits (w - strlen s) z
  | (_, Flat, DBreak(s))  :: z -> fits (w - strlen s) z
  | (_, Fill, DBreak(_))  :: _ -> true
  | (_, Break, DBreak(_)) :: _ -> true
  | (i, _, DGroup(_, x))  :: z -> fits w ((i, Flat, x) :: z)

(* [format] does the actual pretty printing. It turns a [doc] document
   into a simple [sdoc] document *)

let rec format w k = function
  | [] -> SNil
  | (_, _, DNil)         :: z -> format w k z
  | (i, m, DCons (x, y)) :: z -> format w k ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DNest (j, x)) :: z -> format w k ((i + j, m, x) :: z)
  | (_, _, DText s)      :: z -> SText(s , format w (k + strlen s) z)
  | (_, Flat, DBreak s)  :: z -> SText(s , format w (k + strlen s) z)
  | (i, Fill, DBreak s)  :: z -> let l = strlen s in
    if   fits (w - k - l) z
    then SText(s, format w (k + l) z)
    else SLine(i, fun () -> format w i z)
  | (i, Break, DBreak _)       :: z -> SLine(i, fun () -> format w i z)
  | (i, _, DGroup (GFlat, x))  :: z -> format w k ((i, Flat , x) :: z)
  | (i, _, DGroup (GFill, x))  :: z -> format w k ((i, Fill , x) :: z)
  | (i, _, DGroup (GBreak, x)) :: z -> format w k ((i, Break, x) :: z)
  | (i, _, DGroup (GAuto, x))  :: z -> if fits (w - k) ((i, Flat, x) :: z)
    then format w k ((i, Flat , x) :: z)
    else format w k ((i, Break, x) :: z)

let to_string t ~width:w = sdoc_to_string (format w 0 [(0, Flat, agrp t)])

let print t oc ~width:w = sdoc_to_file oc (format w 0 [(0, Flat, agrp t)])

(******************************************************************************)
(* niceties *)

let (^+^) x y = x ^^ break ^^ y

let indent n d = nest n (text (String.make n ' ') ^^ d)

