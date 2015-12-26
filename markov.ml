open Printf 

type distribution =
  { total : int ;
    amounts : (string * int) list } ;;
type htable = (string, distribution) Hashtbl.t ;;

type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t } ;;

let append_buffer buf l =
  let tok = Buffer.contents buf in
  Buffer.clear buf;
  if tok = "" then l else tok :: l

let sentences str =
  let buf = Buffer.create 0
  in let rec aux str (tokens : string list list) idx =
       if String.length str = idx then
         (append_buffer buf (List.hd tokens)) :: List.tl tokens
       else
         match str.[idx] with
         | ';' | ',' | ':' | '-' | '"' | '\'' as t ->
             let new_token = String.make 1 t in
             aux str
               ((new_token :: (append_buffer buf (List.hd tokens))) :: List.tl tokens)
               (idx + 1)
         | '?' | '!' | '.' as t ->
             let new_token = String.make 1 t in
             aux str
               ([] :: ((new_token :: (append_buffer buf (List.hd tokens))) :: List.tl tokens))
               (idx + 1)
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as t ->
             Buffer.add_char buf t;
             aux str tokens (idx + 1)
         | c when Char.code c >= 128 ->
             Buffer.add_char buf c;
             aux str tokens (idx + 1)
         | _ -> (*  anything else is a separator *)
             aux str ((append_buffer buf (List.hd tokens)) :: List.tl tokens) (idx + 1)
  in List.filter (fun l -> List.length l != 0)
                 (List.rev (List.map List.rev (aux str [[]] 0))) ;;

let compute_distribution l =
  let f acc e =
    match acc with
    | (c, count) :: rest  when e = c ->
        (c, count + 1) :: rest
    | (_, _) :: _ ->
        (e, 1) :: acc
    | [] -> [(e, 1)]
  in let dist = List.fold_left f [] (List.sort compare l)
  in { total = List.fold_left (fun acc (_, n) -> acc + n) 0 dist ;
       amounts = dist } ;;

let rec start pl =
  if pl = 0 then [] else "START" :: start (pl - 1) ;;

let shift l y =
  match l with
  | x :: xs -> xs @ [y]
  | _ -> invalid_arg "Empty list" ;;

let build_ptable words pl =
  let table = Hashtbl.create 100 in
  let final_table = Hashtbl.create 100 in
  let rec aux last = function
    | x :: y :: xs ->
       let entry = shift last x in
       let succ =
         try Hashtbl.find table entry with Not_found -> []
       in
       Hashtbl.replace table entry (y :: succ);
       aux entry (y :: xs)
    | [x] ->
       let entry = shift last x in
        let succ =
          try Hashtbl.find table entry with Not_found -> []
        in Hashtbl.replace table entry ("STOP" :: succ)
    | [] -> ()
  in
  let first = match words with
    | x :: xs -> x
    | _ -> "STOP"
  in
  let start_seq = start pl in
  Hashtbl.replace table start_seq [first];
  aux start_seq words;
  Hashtbl.iter (fun key v ->
      Hashtbl.add final_table key (compute_distribution v)) table;
  { prefix_length = pl ; table = final_table };;

let next_in_htable table word =
  let { total; amounts } =
    Hashtbl.find table word
  in
  let ridx = Random.int total in
  let rec sel n = function
    | (c, x) :: tail when x > n -> c
    | (c, x) :: tail -> sel (n - x) tail
    | _ -> invalid_arg "Empty list"
  in sel ridx amounts ;;

let walk_ptable { table ; prefix_length = pl } =
  let rec aux table str last =
    let next = next_in_htable table last in
    if next = "STOP" then str
    else aux table (next :: str) (shift last next)
  in List.rev (aux table [] (start pl))


let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec loop acc =
    match read_line ic with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in
  loop []

let () =
  let random_sentence = walk_ptable (build_ptable
                          (List.flatten
                             (sentences (String.concat "\n" (read_lines stdin)))) 2)
  in List.iter (fun str -> printf "%s " str) random_sentence









