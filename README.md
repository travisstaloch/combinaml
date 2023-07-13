# combinaml

Simple, customizable, dependency free parser combinator library inspired by
[angstrom](https://github.com/inhabitedtype/angstrom/)

# examples

See the [tests](tests/tests.ml) 

If you've used angstrom, you're likely to be very comfortable with this library.
It uses many of the same operators and names.

# api documentation

[combinaml.mli](combinaml/combinaml.mli)

# compared with angstrom

### customizable

combinaml exposes all of its types and functions which allows users to define
custom parsers.  if there is some way to create custom parsers with angstrom,
please open an issue and let me know.

### shared api with angstrom

* 'aka' below refers to the name in angstrom

`return`, `fail`, `<?>`, `>>=`, `>>|`, `<*`, `*>`, `<*>`, `<|>`,
`lift2`..`lift6`, `satisfy`, `char`, `any_char`, `not_char`, `str` (aka
`string`), `forward` (aka `advance`), `is_end_of_input`, `end_of_input`,
`peek_char`, `peek_char_fail`, `peek_string`, `pos`, `len` (aka `available`),
`take_while_fn`, `take_while_fn1` (aka `take_while`), `take_until_fn`,
`take_until_fn1` (aka `take_till`), `scan`, `many`, `many1`, `fix`,
`many_until` (aka `many_till`), `sep_by1`, `sep_by`, `choice`, `list`,
`parse_string`, `both`, `skip_many`, `skip_many1`


### available in angstrom, not in combinaml
`fix_lazy`, `scan_state`, `scan_string`, and many more (TODO document others)

### available in combinaml, not in angstrom
`any_char_in`, `any_char_except`, `char_range`, `backward`, `peek`, `until`,
`many_until`

### different api from angstrom
`option` - always succeeds returning an ocaml option

in combinaml
* these operate with a parser rather than a `char -> bool`:
  `take_while`, `take_until`
* these have optional min and max params: `many`, `take_while`, `take_until`,
  `take_while_fn`, `take_until_fn`
