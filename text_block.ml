open Core.Std

include Int.Replace_polymorphic_compare

type dims = { width : int; height : int }
  (* INVARIANT: width and height are both non-negative *)

type valign = [`Top | `Bottom | `Center]
type halign = [`Left | `Right | `Center]

type t =
  | Text of string
  | Span of char option * dims
  | Hcat of t * t * dims
  | Flip of t * dims (* reflect across the line x = y *)

(* INVARIANTS:
    For each [Text x],
      [x] contains no newlines.
    For each [Hcat (t1, t2, {height = h; width = w})],
      h = height t1 = height t2, and
      w = width t1 + width t2
*)
(* LAWS:
    hcat (hcat t1 t2) t3 = hcat t1 (hcat t2 t3)
    hcat nil t = t = hcat t nil where width nil =
*)

let span ?ch ~width ~height () =
  assert (width >= 0);
  assert (height >= 0);
  Span (ch, {width; height})

let nil = span ~width:0 ~height:0 ()

let height = function
  | Text _ -> 1
  | Span (_, d) | Hcat (_, _, d) | Flip (_, d) -> d.height

let width = function
  | Text s -> String.length s
  | Span (_, d) | Hcat (_, _, d) | Flip (_, d) -> d.width

let flip_dims {width; height} = {width = height; height = width}

let flip = function
  | Flip (t, _) -> t
  | t -> Flip (t, {width = height t; height = width t})

let halve n =
  let fst = n / 2 in
  let snd = fst + n mod 2 in
  (fst, snd)

let rec hpad t ~align delta =
  assert (delta >= 0);
  if delta = 0 then t else begin
    let height = height t in
    let pad = Span (None, {height; width = delta}) in
    match align with
    | `Left   -> Hcat (t, pad, {height; width = width t + delta})
    | `Right  -> Hcat (pad, t, {height; width = width t + delta})
    | `Center ->
      let (delta1, delta2) = halve delta in
      let t = hpad t ~align:`Left  delta1 in
      let t = hpad t ~align:`Right delta2 in
      t
  end

let vpad t ~align delta =
  let align =
    match align with
    | `Top    -> `Left
    | `Bottom -> `Right
    | `Center -> `Center
  in
  flip (hpad (flip t) ~align delta)

let hcat ?(align = `Top) t1 t2 =
  if height t1 = 0 then t2 else
  if height t2 = 0 then t1 else begin
    let (max_height, t1, t2) =
      if height t1 > height t2 then
        (height t1, t1, vpad ~align t2 (height t1 - height t2))
      else if height t2 > height t1 then
        (height t2, vpad ~align t1 (height t2 - height t1), t2)
      else
        (height t1, t1, t2)
    in
    Hcat (t1, t2, {height = max_height; width = width t1 + width t2})
  end

let vcat ?(align = `Left) t1 t2 =
  let align =
    match align with
    | `Left   -> `Top
    | `Right  -> `Bottom
    | `Center -> `Center
  in
  flip (hcat (flip t1) (flip t2) ~align:align)

let text ?(align = `Left) str =
  match String.split str ~on:'\n' with
  | [] -> nil
  | [line] -> Text line
  | line :: lines ->
    let max_width =
      List.fold lines ~init:(String.length line) ~f:(fun acc line ->
        Int.max acc (String.length line))
    in
    let pad line =
      hpad ~align (Text line) (max_width - String.length line)
    in
    List.fold_left lines ~init:(pad line)
      ~f:(fun t line -> vcat ~align t (pad line))

let valign align ts =
  let max_height =
    List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (height t))
  in
  List.map ts ~f:(fun t -> vpad ~align t (max_height - height t))

let halign align ts =
  let max_width =
    List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (width t))
  in
  List.map ts ~f:(fun t -> hpad ~align t (max_width - width t))

let render t =
  let eol_length = 1 in
  let line_length = width t + eol_length in
  let buf = String.make (height t * line_length) ' ' in
  let write_direct c i j = buf.[i + j * line_length] <- c in
  for j = 0 to height t - 1 do write_direct '\n' (width t) j done;
  let write_flipped c j i = write_direct c i j in
  let rec aux t offset write_direct write_flipped =
    match t with
    | Text s ->
      for i = 0 to String.length s - 1 do
        write_direct s.[i] (i + offset.width) offset.height
      done
    | Span (ch, d) ->
      Option.iter ch ~f:(fun ch ->
        for i = 0 to d.width - 1 do
          for j = 0 to d.height - 1 do
            write_direct ch
              (i + offset.width)
              (j + offset.height)
          done
        done)
    | Flip (t, _) ->
      aux t (flip_dims offset) write_flipped write_direct
    | Hcat (t1, t2, _) ->
      aux t1 offset write_direct write_flipped;
      let offset' =
        {height = offset.height; width = offset.width + width t1}
      in
      aux t2 offset' write_direct write_flipped
  in
  aux t {width = 0; height = 0} write_direct write_flipped;
  buf
