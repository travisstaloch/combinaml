module Input : sig
  type t = { s : string; pos : int }

  val init : string -> t

  exception End_of_input

  val show : t -> string

  val length : t -> int
  (** [length i] return remaining length ([String.length i.s - i.pos]). *)

  val is_at_end : t -> bool
  val has_length : t -> int -> bool
  val incr_by_unsafe : int -> t -> t
  (* [incr_by_unsafe len i] return an [Input.t] with it's [pos] incremented by [len]. *)

  val incr_unsafe : t -> t
  (* [incr_unsafe len i] return an [Input.t] with it's [pos] incremented 1. *)

  val incr_by : int -> t -> t
  (* [incr_by len i] return an [Input.t] with it's [pos] incremented by [len]. raises [End_of_input] if beyond input length. *)

  val sub_unsafe : t -> int -> string
  (** [sub_unsafe i len] return sub string of [i.s] from [i.pos..i.pos + len]. *)

  val rest_unsafe : t -> string
  (** [rest_unsafe i] returns the rest of the input from [i.pos]. *)

  val rest : t -> string

  val rest_of_len_unsafe : int -> t -> string
  (** [rest_of_len_unsafe len i] returns the rest of the input from [i.pos .. i.pos + len]. *)

  val rest_of_len : int -> t -> string

  val get_unsafe : int -> t -> char
  (** [get_unsafe pos i] returns the character at position [i.pos + pos]. *)

  val get : int -> t -> char

  val first_unsafe : t -> char
  (** [first_unsafe i] returns the character at position [i.pos]. *)

  val first : t -> char

  val take_while_fn : t -> ?max:int -> (char -> bool) -> int
  (** [take_while_fn i max f] returns a sub string from [i.pos] to the first character where [f c] fails. *)

  val starts_with : t -> string -> bool
  (** [starts_with i s] returns true if input starts with s.  does not create any temporary strings by using String.sub. *)
end

type err = int * string
(** [err] an input position and message. *)

type errs = err list

type fail = Input.t -> errs -> errs
(** function which may cons errs. *)

type 'a res = Input.t * ('a, fail) result
type 'a t = { run : Input.t -> fail -> 'a res }

val input : string -> Input.t
(** [input s] alias of Input.init *)

val return : 'a -> 'a t
(** [return x] a parser that always succeeds, returning x *)

val errors_to_string : errs -> (err -> string) -> string
(** [errors_to_string errs show_err] a helper for converting [errs] to a string. *)

val fail : string -> 'a t
(** [fail msg] a parser that always fails with [msg]. *)

val ( <?> ) : 'a t -> string -> 'a t
(** [p <?> msg] provide a failure [msg] for parser [p]. *)

val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
(** [p >>| f] map operator. *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> p] reverse map. *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [a >>= ab] bind operator. *)

val ( *> ) : 'a t -> 'b t -> 'b t
(** [a *> b] run [a] and [b] in sequence ignoring result of [a]. *)

val ( <* ) : 'a t -> 'b t -> 'a t
(** [a <* b] run [a] and [b] in sequence ignoring result of [b]. *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** [ab <*> a] run [ab] and [a] in sequence, passing the result of [a] to [ab]. *)

val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [lift f p1 p2] run [p1] and [p2] in sequence, applying [f] to the results. *)

val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t

val lift5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t

val lift6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t

val satisfy : (char -> bool) -> char t
(** [satisfy f] a parser that accepts when [f] returns true. *)

val char : char -> char t
(** [char c] a parser accepting [c]. *)

val not_char : char -> char t
(** [not_char c] a parser accepting not [c]. *)

val any_char : char t
(** [any_char] a parser accepting any char. *)

val any_char_in : string -> char t
(** [any_char_in s] a parser accepting any char in [s]. *)

val any_char_except : string -> char t
(** [any_char_except s] a parser accepting any char not in [s]. *)

val char_range : char -> char -> char t
(** [char_range min max] a parser accepting any char in the range [min max) (inclusive). *)

val str : string -> string t
(** [str s] a parser accepting input which starts with [s]. *)

val forward : int -> unit t
(** [forward n] move the input position forward by [n]. fail if resulting input position would be out of bounds. *)

val backward : int -> unit t
(** [backward n] move the input position backward by [n]. fail if resulting input position would be less than 0. *)

val is_end_of_input : bool t
(** [is_end_of_input] always succeeds. true if at end of input. *)

val end_of_input : unit t
(** [end_of_input] a parser accepting when at end of input. *)

val peek_char : char option t
(** [peek_char] return optional next char without advancing input. *)

val peek_char_fail : char t
(** [peek_char_fail] return next char without advancing input. fail on end of input. *)

val peek : 'a t -> 'a t
(** [peek] run p without advancing input. *)

val peek_string : int -> string t
(** [peek_string] return next n characters without advancing input. *)

val pos : int t
(** [pos] return current input position. *)

val len : int t
(** [len] return current input remaining length. *)

val take_while : ?min:int -> ?max:int -> 'a t -> string t
(** [take_while ?min ?max p] a parser that takes input while [p] succeeds up to [max] times. fails if run less than [min] times. *)

val take_while1 : 'a t -> string t

val take_until : ?min:int -> ?max:int -> 'a t -> string t
(** [take_until ?min ?max p] a parser that takes input until [p] succeeds up to [max] times. fails if run less than [min] times.  *)

val take_until1 : 'a t -> string t

val take_while_fn : ?min:int -> ?max:int -> (char -> bool) -> string t
(** [take_while_fn ?min ?max f] take up to [max] chars of input while [f] is true. fails if run less than [min] times. *)

val take_while_fn1 : ?max:int -> (char -> bool) -> string t

val take_until_fn : ?min:int -> ?max:int -> (char -> bool) -> string t
(** [take_until_fn ?min ?max f] take up to [max] chars of input while [f] is false. fails if run less than [min] times. *)

val take_until_fn1 : ?max:int -> (char -> bool) -> string t

val scan : 'a -> ('a -> char -> 'a option) -> (string * 'a) t
(** [scan state f] given initial [state], take input while [f] returns Some. returns string * final state. *)

val option : 'a t -> 'a option t
(** [optional] always succeeds returning result as an optional. *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** [p1 <|> p2] alternative operator. if [p1] fails, run [p2]. both [p1] and [p2] receive the same input. *)

val many : ?min:int -> ?max:int -> 'a t -> 'a list t
(** [many ?min ?max p] return a list from evaluating [p] up to [max] times. fails if run less than [min] times. *)

val many1 : 'a t -> 'a list t

val fix : ('a t -> 'a t) -> 'a t
(** [fix p] create a lazy version of [p]. this allows for creating recursive parsers. *)

val until : 'b t -> 'a t -> 'b t
(** [until p end] take input until the start of [end] (without consuming [end]) and feed the result to [p]. *)

val many_until : 'a t -> 'b t -> 'a list t
(** [many_until p end] accumulate result of [p] in a list until [end] succeeds. consumes and ignores [end]. *)

val sep_by1 : 'a t -> 'b t -> 'b list t
(** [sep_by sep p] accumulate result of [(p (sep p)?)+] in a list. *)

val sep_by : 'a t -> 'b t -> 'b list t
(** [sep_by sep p] accumulate result of [(p (sep p)?)*] in a list. *)

val choice : ?failure_msg:string -> 'a t list -> 'a t
(** [choice ps] run [ps] in order with same input. return the first success. *)

val list : 'a t list -> 'a list t
(** [list ps] run [ps] in sequence. return a list of their results. *)

val skip_many : ?min:int -> ?max:int -> 'a t -> unit t
(** [skip_many ?min ?max p] run [p] up to [max] times, discarding result. fails if run less than [min] times. *)

val skip_many1 : ?max:int -> 'a t -> unit t

type consume = Prefix | All

val parse_string : ?consume:consume -> 'a t -> string -> 'a res
(** [parse_string ?consume p s] return result of running [p] on [s]. [consume:All] requires that all input is consumed by running [p <* end_of_input]. *)

val pair : 'a -> 'b -> 'a * 'b
(** [pair a b] a helper useful with lift2. *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** [both a b] run a and b in sequence returning their result as a tuple *)
