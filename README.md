## Corrections, clarifications, links, & notes

- Related links:
  - [Rust Boolean methods for `Result`](https://doc.rust-lang.org/std/result/#boolean-operators)
  - [gingerBill of Odin on fancy Booleans](https://www.gingerbill.org/article/2021/09/06/value-propagation-experiment-part-2/)
- In ECMAScript, `?.` is called "optional chaining", and in Kotlin is called
  the "safe call operator".
- I also could have used the ECMAScript `??` nullish coalescing operator to be
  more precise in my `undefined` handling than what I was doing with `||`.
- As in any other language, I could have created structured optional and result
  types in TypeScript to mimic those of Rust, but I opted for the simpler
  unwrapped method here.
- In Zig and Odin, I gave a poor analysis of wrapped optionals and failable
  results. One improved analysis would be to see where nesting is possible. For
  example, can a nest one optional inside another, or do the idiomatic language
  features not support this?
- In Odin, I could have used a `distinct` string for my error type rather than
  putting it inside a struct, but I liked seeing the extra context in the
  output that I got from the struct.
- In Kotlin, the "never" type is called `Nothing`.


## Quote of the day

From the NeverValued Story:

> A `null` would be something. Nah, this was `Nothing`.
