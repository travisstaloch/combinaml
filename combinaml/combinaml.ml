module Input = struct
  type t = { s : string; pos : int }

  let init s = { s; pos = 0 }

  exception End_of_input

  let show i = Printf.sprintf "%d/%d" i.pos (String.length i.s)

  let length i =
    let res = String.length i.s - i.pos in
    res

  let is_at_end i = length i <= 0
  let has_length i n = length i >= n
  let incr_by_unsafe (len : int) (i : t) : t = { i with pos = i.pos + len }
  let incr_unsafe = incr_by_unsafe 1

  let incr_by (len : int) (i : t) : t =
    if len > length i then raise End_of_input;
    incr_by_unsafe len i

  let sub_unsafe i len =
    let r = Bytes.create len in
    String.unsafe_blit i.s i.pos r 0 len;
    Bytes.unsafe_to_string r

  let rest_unsafe (i : t) : string = sub_unsafe i (String.length i.s - i.pos)

  let rest (i : t) : string =
    if i.pos > String.length i.s then raise End_of_input;
    sub_unsafe i (String.length i.s - i.pos)

  let rest_of_len_unsafe len (i : t) : string =
    let s = sub_unsafe i len in
    s

  let rest_of_len len (i : t) : string =
    if len < length i then raise End_of_input;
    rest_of_len_unsafe len i

  let get_unsafe pos (i : t) : char = String.unsafe_get i.s (pos + i.pos)

  let get pos (i : t) : char =
    let len = length i in
    if pos >= len then raise End_of_input;
    get_unsafe pos i

  let first_unsafe = get_unsafe 0
  let first = get 0

  let take_while_fn i ?(max = Int.max_int) f =
    let pos = ref i.pos in
    let limit = String.length i.s in
    while !pos < limit && f (String.get i.s !pos) do
      incr pos
    done;
    !pos - i.pos

  let starts_with i prefix =
    let len = String.length prefix in
    let ilen = length i in
    if ilen < len then false
    else
      let n = ref 0 in
      while
        !n < len
        &&
        let c = get_unsafe !n i in
        let pc = String.unsafe_get prefix !n in
        c = pc
      do
        incr n
      done;
      !n = len
end

type err = int * string
type errs = err list
type fail = Input.t -> errs -> errs
type 'a res = Input.t * ('a, fail) result
type 'a t = { run : Input.t -> fail -> 'a res }

let input = Input.init
let return a : 'a t = { run = (fun i fail -> i, Ok a) }

let errors_to_string es : string =
  List.map (fun (pos, msg) -> Printf.sprintf "::%d %s" pos msg) es
  |> String.concat "\n"

let fail msg : 'a t =
  {
    run =
      (fun i fail ->
        let fail' (i : Input.t) errs = (i.pos, msg) :: errs in
        i, Error fail');
  }

let ( <?> ) p msg : 'a t =
  {
    run =
      (fun i fail ->
        let fail' (i' : Input.t) errs = fail i ((i'.pos, msg) :: errs) in
        p.run i fail');
  }

let ( >>| ) (p : 'a t) (f : 'a -> 'b) : 'b t =
  {
    run =
      (fun i fail ->
        match p.run i fail with
        | i', Ok a -> i', Ok (f a)
        | i', Error e -> i', Error e);
  }

let ( <$> ) f p = p >>| f

let ( >>= ) (p : 'a t) (f : 'a -> 'b t) : 'b t =
  {
    run =
      (fun i fail ->
        match p.run i fail with
        | i', Ok a -> (f a).run i' fail
        | i', Error e -> i', Error e);
  }

(* equivalent to: let ( *> ) a b = a >>= fun _ -> b *)

let ( *> ) a b =
  {
    run =
      (fun i fail ->
        match a.run i fail with
        | i', Error e -> i', Error e
        | i', Ok _ -> b.run i' fail);
  }

(* equivalent to: let ( <* ) a b =
   a >>= fun x ->
   b >>| fun _ -> x *)

let ( <* ) a b =
  {
    run =
      (fun i fail ->
        match a.run i fail with
        | i', Error e -> i', Error e
        | i', Ok a -> (
            match b.run i' fail with
            | i'', Error fail -> i'', Error fail
            | i'', Ok _ -> i'', Ok a));
  }

(* equivalent to: let ( <*> ) f m = f >>= fun f -> m >>| f *)

let ( <*> ) ab a =
  {
    run =
      (fun i fail ->
        let i', r = ab.run i fail in
        match r with
        | Error e -> i', Error e
        | Ok atob -> (
            match a.run i' fail with
            | i'', Error e -> i'', Error e
            | i'', Ok a -> i'', Ok (atob a)));
  }

(* equivalent to let lift2 f x y = f <$> x <*> y *)

let lift2 f p1 p2 =
  {
    run =
      (fun i fail ->
        let i, r = p1.run i fail in
        match r with
        | Error e -> i, Error e
        | Ok a -> (
            let i, r = p2.run i fail in
            match r with Ok b -> i, Ok (f a b) | Error e -> i, Error e));
  }

let lift3 f p1 p2 p3 =
  {
    run =
      (fun i fail ->
        let i, r = p1.run i fail in
        match r with
        | Error e -> i, Error e
        | Ok a -> (
            let i, r = p2.run i fail in
            match r with
            | Error e -> i, Error e
            | Ok b -> (
                let i, r = p3.run i fail in
                match r with Error e -> i, Error e | Ok c -> i, Ok (f a b c))));
  }

let lift4 f p1 p2 p3 p4 =
  {
    run =
      (fun i fail ->
        let i, r = p1.run i fail in
        match r with
        | Error e -> i, Error e
        | Ok a -> (
            let i, r = p2.run i fail in
            match r with
            | Error e -> i, Error e
            | Ok b -> (
                let i, r = p3.run i fail in
                match r with
                | Error e -> i, Error e
                | Ok c -> (
                    let i, r = p4.run i fail in
                    match r with
                    | Error e -> i, Error e
                    | Ok d -> i, Ok (f a b c d)))));
  }

let lift5 f p1 p2 p3 p4 p5 =
  {
    run =
      (fun i fail ->
        let i, r = p1.run i fail in
        match r with
        | Error e -> i, Error e
        | Ok a -> (
            let i, r = p2.run i fail in
            match r with
            | Error e -> i, Error e
            | Ok b -> (
                let i, r = p3.run i fail in
                match r with
                | Error e -> i, Error e
                | Ok c -> (
                    let i, r = p4.run i fail in
                    match r with
                    | Error e -> i, Error e
                    | Ok d -> (
                        let i, r = p5.run i fail in
                        match r with
                        | Error e -> i, Error e
                        | Ok e -> i, Ok (f a b c d e))))));
  }

let lift6 f p1 p2 p3 p4 p5 p6 =
  {
    run =
      (fun i fail ->
        let i, r = p1.run i fail in
        match r with
        | Error e -> i, Error e
        | Ok a -> (
            let i, r = p2.run i fail in
            match r with
            | Error e -> i, Error e
            | Ok b -> (
                let i, r = p3.run i fail in
                match r with
                | Error e -> i, Error e
                | Ok c -> (
                    let i, r = p4.run i fail in
                    match r with
                    | Error e -> i, Error e
                    | Ok d -> (
                        let i, r = p5.run i fail in
                        match r with
                        | Error e -> i, Error e
                        | Ok e -> (
                            let i, r = p6.run i fail in
                            match r with
                            | Error e -> i, Error e
                            | Ok g -> i, Ok (f a b c d e g)))))));
  }

let satisfy f : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else
          let c = Input.first_unsafe i in
          if f c then Input.incr_unsafe i, Ok c else i, Error fail);
  }

let char c : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else if c = Input.first_unsafe i then Input.incr_unsafe i, Ok c
        else i, Error fail);
  }

let not_char c : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else if c != Input.first_unsafe i then Input.incr_unsafe i, Ok c
        else i, Error fail);
  }

let any_char : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else Input.incr_unsafe i, Ok (Input.first_unsafe i));
  }

let any_char_in s : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else
          let c = Input.first_unsafe i in
          if not (String.contains s c) then i, Error fail
          else Input.incr_unsafe i, Ok c);
  }

let any_char_except s : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else
          let c = Input.first_unsafe i in
          if String.contains s c then i, Error fail
          else Input.incr_unsafe i, Ok c);
  }

let char_range min max : char t =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else
          let c = Input.first_unsafe i in
          (* branchless. but needs unsigned subtraction *)
          (*
             let a, b, c = (Char.code a, Char.code b, Char.code c) in
                let cmp2 = c - a < b - a + 1 in
          *)
          if min <= c && c <= max then Input.incr_unsafe i, Ok c
          else i, Error fail);
  }

let str (s : string) : string t =
  {
    run =
      (fun i fail ->
        let len = String.length s in
        if len = 0 || not (Input.has_length i len) then i, Error fail
        else if Input.starts_with i s then Input.incr_by_unsafe len i, Ok s
        else i, Error fail);
  }

let forward n =
  {
    run =
      (fun i fail ->
        if not (Input.has_length i n) then i, Error fail
        else Input.incr_by_unsafe n i, Ok ());
  }

let backward n =
  {
    run =
      (fun i fail ->
        if i.pos - n < 0 then i, Error fail
        else Input.incr_by_unsafe (-n) i, Ok ());
  }

let is_end_of_input = { run = (fun i fail -> i, Ok (Input.is_at_end i)) }

let end_of_input =
  {
    run = (fun i fail -> if Input.is_at_end i then i, Ok () else i, Error fail);
  }

let peek_char =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Ok None
        else i, Ok (Some (Input.first_unsafe i)));
  }

let peek_char_fail =
  {
    run =
      (fun i fail ->
        if Input.is_at_end i then i, Error fail
        else i, Ok (Input.first_unsafe i));
  }

let peek p =
  {
    run =
      (fun i fail ->
        let i', r = p.run i fail in
        match r with Error e -> i, Error e | a -> i, a);
  }

let peek_string n : 'a t =
  {
    run =
      (fun i fail ->
        if not (Input.has_length i n) then i, Error fail
        else i, Ok (Input.rest_of_len_unsafe n i));
  }

let pos : 'a t = { run = (fun i fail -> i, Ok i.pos) }
let len : 'a t = { run = (fun i fail -> i, Ok (String.length i.s - i.pos)) }

let take_while ?(min = 0) ?(max = Int.max_int) p =
  {
    run =
      (fun i fail ->
        let i' = ref i in
        let count = ref 0 in
        while
          (not (Input.is_at_end !i'))
          && !count < max
          &&
          let i'', r = p.run !i' fail in
          i' := i'';
          match r with Error e -> false | Ok a -> true
        do
          incr count
        done;
        if !count >= min then
          let len = !i'.pos - i.pos in
          let s = String.sub i.s i.pos len in
          !i', Ok s
        else i, Error fail);
  }

let take_while1 p = take_while ~min:1 p

let take_until ?(min = 0) ?(max = Int.max_int) p =
  {
    run =
      (fun i fail ->
        let i' = ref i in
        let count = ref 0 in
        while
          (not (Input.is_at_end !i'))
          && !count < max
          &&
          let i'', r = p.run !i' fail in
          match r with
          | Error e ->
              i' :=
                {
                  i with
                  pos =
                    i''.pos
                    (* incr by one if p didn't change pos
                       to ensure forward progress *)
                    + Bool.to_int (i''.pos = !i'.pos);
                };
              true
          | Ok a -> false
        do
          incr count
        done;
        let len = !i'.pos - i.pos in
        if !count >= min then
          let s = String.sub i.s i.pos len in
          !i', Ok s
        else i, Error fail);
  }

let take_until1 p = take_until ~min:1 p

let take_while_fn ?(min = 0) ?(max = Int.max_int) f =
  {
    run =
      (fun i fail ->
        let len = Input.take_while_fn ~max i f in
        if len < min then i, Error fail
        else Input.incr_by_unsafe len i, Ok (Input.sub_unsafe i len));
  }

let take_while_fn1 = take_while_fn ~min:1

let scan state f =
  {
    run =
      (fun i fail ->
        let state = ref state in
        let parser =
          take_while_fn (fun c ->
              match f !state c with
              | None -> false
              | Some state' ->
                  state := state';
                  true)
          >>| fun x -> x, !state
        in
        parser.run i fail);
  }

let option (p : 'a t) : 'a option t =
  {
    run =
      (fun i fail ->
        match p.run i fail with
        | i', Ok a -> i', Ok (Some a)
        | i', Error _ -> i', Ok None);
  }

let ( <|> ) p1 p2 : 'a t =
  {
    run =
      (fun i fail ->
        match p1.run i fail with
        | (_, Ok _) as r -> r
        | _, Error fail' -> (
            match p2.run i fail with
            | (_, Ok _) as r2 -> r2
            | _, Error fail'' ->
                let fail (i : Input.t) errs : errs =
                  fail' i [] @ fail'' i errs
                in
                i, Error fail));
  }

let many ?(min = 0) ?(max = Int.max_int) p =
  {
    run =
      (fun i fail ->
        let rec loop acc n i : Input.t * 'a list * int * fail =
          if n = max then i, acc, n, fail
          else
            let i', r = p.run i fail in
            match r with
            | Error fail -> i', acc, n, fail
            | Ok a -> loop (a :: acc) (n + 1) i'
        in
        let i', r, n, fail' = loop [] 0 i in
        let has_min = min <= n in
        let has_max = n <= max in
        if has_min && has_max then i', Ok (r |> List.rev)
        else
          let fail' =
            if not has_min then fun (_ : Input.t) errs ->
              (i'.pos, "not enough matches") :: fail' i' errs
            else fun (_ : Input.t) errs ->
              (i'.pos, "too many matches") :: fail' i' errs
          in
          i, Error fail');
  }

let many1 p = many ~min:1 p

(* TODO fix signature *)
let fix_direct (f : 'a -> 'b) : 'a =
  let rec p = lazy (f r)
  and r = { run = (fun i fail -> (Lazy.force p).run i fail) } in
  (* match r with i', Ok a -> (i', Ok a) | i', Error e -> (i', Error e) *)
  r

(* let fix_lazy f =
   let max_steps = 20 in
   let steps = ref max_steps in
   let rec p = lazy (f r)
   and r =
     {
       run =
         (fun i fail ->
           decr steps;
           if !steps < 0 then (
             steps := max_steps;
             lazy ((Lazy.force p).run i fail))
           else (Lazy.force p).run i fail);
     }
   in
   r *)

let fix = fix_direct

let until sep p : 'a t =
  {
    run =
      (fun i fail ->
        let i', r = (take_until sep).run i fail in
        match r with
        | Error e -> i', Error e
        | Ok s ->
            let _, r' = p.run (input s) fail in
            Input.incr_by_unsafe (i'.pos - i.pos) i, r');
  }

let many_until sep p = fix (fun m -> sep *> return [] <|> lift2 List.cons p m)
let sep_by1 sep p = fix (fun m -> lift2 List.cons p (sep *> m <|> return []))

let sep_by sep p =
  lift2 List.cons p (sep *> sep_by1 sep p <|> return []) <|> return []

let choice ?(failure_msg = "no more choices") ps =
  List.fold_right ( <|> ) ps (fail failure_msg)

let rec list ps =
  match ps with [] -> return [] | p :: ps -> lift2 List.cons p (list ps)

type consume = Prefix | All

let parse_string ?(consume = All) p s =
  let p =
    match (consume : consume) with Prefix -> p | All -> p <* end_of_input
  in
  let i = input s in
  let fail i s = s in
  p.run i fail

let pair x y = x, y
