# combinaml

Simple, customizable, dependency free parser combinator library inspired by
[angstrom](https://github.com/inhabitedtype/angstrom/)

# examples

See the [tests](tests/tests.ml) 

If you've used angstrom, you're likely to be very comfortable with this library.
It uses many of the same operators and names.

# api documentation

[combinaml.mli](combinaml/combinaml.mli)

# compared to angstrom

### customizable

combinaml exposes all of its' types and functions which allows users to define
custom parsers.  if there is some way to do this with angstrom, please open an
issue and let me know.

### shared api with angstrom
`return`, `fail`, `<?>`, `>>=`, `>>|`, `<*`, `*>`, `<*>`, `<|>`,
`lift2`..`lift6`, `satisfy`, `char`, `satisfy`, `any_char`, `not_char`, `str`
(`string` in angstrom), `forward` (`advance` in angstrom), `is_end_of_input`,
`end_of_input`, `peek_char`, `peek_char_fail`, `peek_string`, `pos`, `len`
(`available` in angstrom), `take_while_fn`, `take_while_fn1` (`take_while` in
angstrom), `scan`, `many`, `many1`, `fix`, `many_until` (`many_till` in
angstrom), `sep_by1`, `sep_by`, `choice`, `list`, `parse_string`

### available in angstrom, not in combinaml
`take_until_fn`, `take_until_fn1`, `fix_lazy`

### available in combinaml, not in angstrom
`any_char_in`, `any_char_except`, `char_range`, `backward`, `peek`, `until`,
`many_until`

### different api from angstrom
`optional` - always succeeds returning an ocaml option

in combinaml
* the following operate with a parser rather than a `char -> bool`:
  `take_while`, `take_until`
* these have optional min and max params: `many`, `take_while`, `take_until`,
  `take_while_fn`,
