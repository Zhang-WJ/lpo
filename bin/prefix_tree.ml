open Core

module type Letter = sig
  type t [@@deriving sexp]

  val compare : t -> t -> int
end

module type PersistentSet = sig
  type elt
  type t [@@deriving sexp]

  val empty : t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val remove : elt -> t -> t
  val inter : t -> t -> t
  val compare : t -> t -> int
end

module Make (L : Letter) : PersistentSet with type elt = L.t list = struct
  module M = Map.Make (L) [@@deriving sexp]

  (* A prefix tree is a set of lists, where each list is a sequence of letters.
     The empty list represents the empty word, which can be a member of the set. *)

  type elt = L.t list

  type t =
    { word : bool (* Indicates if the empty list is a word in the set *)
    ; branches : t M.t
    }
  [@@deriving sexp]

  let empty = { word = false; branches = M.empty }
  let is_empty t = (not t.word) && Map.is_empty t.branches

  let rec mem x t =
    match x with
    | [] -> t.word
    | h :: t' ->
      (try mem t' (Map.find_exn t.branches h) with
       | Not_found_s _ -> false)
  ;;

  let rec remove x t =
    match x with
    | [] -> { t with word = false }
    | h :: t' ->
      (try
         let s = remove t' (Map.find_exn t.branches h) in
         let new_branches =
           if is_empty s
           then Map.remove t.branches h
           else Map.update t.branches h ~f:(fun _ -> s)
         in
         { t with branches = new_branches }
       with
       | Not_found_s _ -> t)
  ;;

  let rec add x t =
    match x with
    | [] -> if t.word then t else { t with word = true }
    | h :: t' ->
      let b =
        try Map.find_exn t.branches h with
        | Not_found_s _ -> empty
      in
      let data = add t' b in
      { t with branches = Map.update t.branches h ~f:(fun _ -> data) }
  ;;

  let rec remove x t =
    match x with
    | [] -> { t with word = false }
    | h :: t' ->
      (try
         let b = remove t' (Map.find_exn t.branches h) in
         let new_branches =
           if is_empty b
           then Map.remove t.branches h
           else Map.update t.branches h ~f:(fun _ -> b)
         in
         { t with branches = new_branches }
       with
       | Not_found_s _ -> t)
  ;;

  let rec inter t1 t2 =
    { word = t1.word && t2.word; branches = inter_branches t1.branches t2.branches }

  and inter_branches m1 m2 =
    Map.fold m1 ~init:M.empty ~f:(fun ~key:i ~data:ti m ->
      try
        let t = inter ti (Map.find_exn m2 i) in
        if is_empty t then m else Map.update m i ~f:(fun _ -> t)
      with
      | Not_found_s _ -> m)
  ;;

  let rec compare t1 t2 =
    let c = Stdlib.compare t1.word t2.word in
    if c <> 0 then c else M.compare compare t1.branches t2.branches
  ;;
end

module Char = struct
  type t = char [@@deriving sexp]

  let compare = Stdlib.compare
end

module CharTre = Make (Char)

let s = CharTre.empty
let s = CharTre.add [ 'a'; 'c' ] s

let%expect_test _ =
  CharTre.mem [ 'a' ] s |> Bool.to_string |> print_endline;
  [%expect "false"]
;;

let%expect_test _ =
  CharTre.mem [ 'a'; 'c' ] s |> Bool.to_string |> print_endline;
  [%expect "true"]
;;
