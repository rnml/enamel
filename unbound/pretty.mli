(* originally by Christian Lindig

  This module provides a pretty printer.  Documents are built from a set of primitives
  which later can be formatted for different line length.  The pretty printer is intended
  for tree like structures like source code.  It does not provide a two dimensional layout
  like TeX for example.  This pretty printer is heavily inspired by a pretty printer
   proposed by Philip Wadler in his paper "A prettier printer" available from his home page
   and to appear in the Journal of Functional Programming in 1999.
*)

(* documents which can be pretty printed. *)
type t

(* the empty document. *)
val empty : t

(* [d1 ^^ d2] is the concatenation of documents [d1] and [d2]. *)
val (^^) : t -> t -> t

(* [d1 ^+^ d2] equals [d1 ^^ break ^^ d2]. *)
val (^+^) : t -> t -> t

(* [text "hello"] creates a document containing the string ["hello"].  This operator
   simply turns strings into documents. *)
val text : string -> t

(* [break] represents a point where the pretty printer can decide whether to begin a new
   line or not. In the former case a newline is printed, followed by a number of spaces to
   indent the next line.  The number of spaces is controlled by the [nest] operator. When
   the pretty printer does not begin a new line a single space is printed for the
   [break]. *)
val break : t

(* [break_with s] acts like [break] except that it is not turned into a space or a newline
   but into [s] or a newline (followed by spaces). *)
val break_with : string -> t

(* [nest i doc]: all breaks inside [doc] which are turned into newlines will be followed
   by additional [i] spaces. Nesting adds up: when [doc] contains another [nest j]
   operator the breaks inside its document will followed by [i+j] spaces. *)
val nest : int -> t -> t

val indent : int -> t -> t

val hgrp : t -> t
(* [break]s inside a [hgrp] (horizontal group) are never turned into newlines, so they
   always come out as spaces. *)

(* [break]s inside a [vgrp] (vertical group) are always turned onto newlines (which are
   followed by spaces as indicated by [nest]). *)
val vgrp : t -> t

(* The automatic group [agrp] is the most versatile: when the whole group including all
   subgroups fits into one line [break]s come out as spaces.  Otherwise [break]s come out
   as newlines.  However, this does not affect subgroups: their [break]s are considered
   separately. *)
val agrp : t -> t

(* The break policy inside an [agrp] is fixed for all breaks of the group. Inside a
   flexible group [fgrp] each [break] is considered individually: when the document up to
   the next [break] fits into the current line the [break] comes out as space. Otherwise
   it comes out as newline followed by spaces. *)
val fgrp : t -> t

(* [pp_to_string doc ~width] formats [doc] for line length [width] (> 0) and returns it as
   string. This is not efficient and should be avoided for large documents.  The document
   is considered to be surrounded by a virtual [agrp] - if this is not desired, it should
   have itself a different group at its outermost level to protect itself from the
   [agrp]. *)
val to_string : t -> width:int -> string

(* [print doc cout ~width] pretty prints [doc] to [cout] for line length [width].  The
   document is considered to be surrounded by a virtual [agrp] - if this is not desired,
   it should have itself a different group at its outermost level to protect itself from
   this [agrp]. *)
val print : t -> out_channel -> width:int -> unit

