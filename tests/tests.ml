open Combinaml

module Fmt = struct
  let pr = Printf.printf
end

let print_res ((i, res) : 'a res) =
  (match res with
  | Ok (s : string) -> Fmt.pr "%s" s
  | Error fail -> Fmt.pr "%s" (errors_to_string (fail i [])));
  Fmt.pr ",%s" (Input.rest i)

let print_result s (p : 'a t) = p.run (input s) (fun i s -> s) |> print_res
let char_to_str_f = String.make 1
let char_to_str (p : 'a t) = p >>| char_to_str_f
let chars_to_str (p : 'a t) = p >>| fun cs -> cs |> List.to_seq |> String.of_seq

let opt_char_to_str (p : 'a option t) =
  p >>| fun o -> match o with Some c -> char_to_str_f c | None -> ""

let%expect_test "basic" =
  let s = "foobar" in
  print_result s (char 'f' |> char_to_str);
  [%expect "f,oobar"];
  print_result s (char 'b' <?> "'b' failed" |> char_to_str);
  [%expect {|::0 'b' failed,foobar|}];
  print_result s (str "foo");
  [%expect "foo,bar"];
  print_result s (str "bar" <?> "'bar' failed");
  [%expect {|::0 'bar' failed,foobar|}];
  print_result s (str "foo" >>| Fun.const "bar");
  [%expect "bar,bar"];
  print_result s (str "foo" >>= Fun.const (return "bar"));
  [%expect "bar,bar"];
  print_result s (str "foo" *> str "bar");
  [%expect "bar,"];
  print_result s (str "foo" <* str "bar");
  [%expect "foo,"];
  print_result s (str "foo" <|> str "baz");
  [%expect "foo,bar"];
  print_result s (str "baz" <|> str "foo");
  [%expect "foo,bar"];
  print_result s
    (str "baz" <?> "'baz' failed" <|> (str "fiz" <?> "'fiz' failed"));
  [%expect {|
::0 'baz' failed
::0 'fiz' failed,foobar |}];
  let join2_chars a b = String.make 1 a ^ String.make 1 b in
  print_result s (join2_chars <$> char 'f' <*> char 'o');
  [%expect "fo,obar"];
  print_result s (lift2 join2_chars (char 'f') (char 'o'));
  [%expect "fo,obar"];
  let ident_start c =
    match c with 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false
  in
  let ident = many (satisfy ident_start) in
  print_result s (ident |> chars_to_str);
  [%expect "foobar,"];
  print_result s (fail "error: foo");
  [%expect "::0 error: foo,foobar"];
  print_result s (many (char_range 'a' 'z') |> chars_to_str);
  [%expect "foobar,"];
  print_result s (option (char 'z') |> opt_char_to_str);
  [%expect ",foobar"];
  print_result s (take_while_fn1 ident_start);
  [%expect "foobar,"];
  print_result s (take_while_fn1 (fun c -> false) <?> "take_while_fn failed");
  [%expect "::0 take_while_fn failed,foobar"];
  print_result s (take_while1 (satisfy ident_start));
  [%expect "foobar,"];
  print_result s (many_until (char 'b') (satisfy ident_start) |> chars_to_str);
  [%expect "foo,ar"];
  print_result s (many1 (char 'b') <?> "'b' failed" |> chars_to_str);
  [%expect "\n::0 not enough matches\n::0 'b' failed,foobar"];
  print_result s (many1 (char 'f') |> chars_to_str);
  [%expect "f,oobar"];
  print_result s (many ~max:3 any_char |> chars_to_str);
  [%expect "foo,bar"];
  print_result s
    (many ~min:1 (str "foo" *> char 'z') <?> "'z' failed" |> chars_to_str);
  [%expect "\n::3 not enough matches\n::3 'z' failed,foobar"];
  print_result "1,2foo" (sep_by1 (char ',') (char_range '0' '9') |> chars_to_str);
  [%expect "12,foo"];
  print_result s (peek (str "foo" <* str "bar"));
  [%expect "foo,foobar"];
  print_result s (take_until1 (str "bar"));
  [%expect "foo,bar"];
  let spaces = take_while (char ' ') in
  let ident = take_while1 (satisfy ident_start) <* spaces in
  let larrow = str "<-" <* spaces in
  let ident_larrow = ident <* larrow in
  let def = lift2 pair ident_larrow (until ident_larrow (many ident)) in
  let def_to_string (n, ss) = n ^ " <- " ^ String.concat " " ss in
  let defs_to_string defs = List.map def_to_string defs |> String.concat "," in
  print_result "a <-b c d <- e" (many def >>| defs_to_string);
  [%expect "a <- b c,d <- e,"];
  print_result s (char 'f' *> pos >>| fun d -> Printf.sprintf "%d" d);
  [%expect "1,oobar"];
  print_result s (char 'f' *> len >>| fun d -> Printf.sprintf "%d" d);
  [%expect "5,oobar"];
  print_result s (choice [ char 'f'; char 'o' ] |> char_to_str);
  [%expect "f,oobar"];
  print_result s (list [ char 'f'; char 'o' ] |> chars_to_str);
  [%expect "fo,obar"];

  ()
