An example of what factoring out
[`blake3::Hash`](https://docs.rs/blake3/latest/blake3/struct.Hash.html) into
its own crate might look like. This could be shared with e.g.
[`blake2b_simd::Hash`](https://docs.rs/blake2b_simd/latest/blake2b_simd/struct.Hash.html).

Current problems:
- It probably makes sense to make the size generic, rather than assuming 32
  bytes. Both 32 bytes and 64 bytes are common hash sizes.
- The `.to_hex()` method wants to return a hex encoded `ArrayString` of twice
  the size, but multiplying by two in a const generic argument is currently a
  nightly-only feature,
  [`generic_const_exprs`](https://doc.rust-lang.org/beta/unstable-book/language-features/generic-const-exprs.html).
- Even with that feature turned on, we need to insert some unfortunate `Sized`
  bounds to avoid getting compiler errors that look like this:

  ```
    error: unconstrained generic constant
     --> src/lib.rs:214:44
      |
  214 |             serializer.serialize_str(&self.to_hex())
      |                                            ^^^^^^
      |
      = help: try adding a `where` bound using this expression: `where [(); { 2 * N }]:`
  note: required by a bound in `Hash::<N>::to_hex`
     --> src/lib.rs:56:41
      |
  56  |     pub fn to_hex(&self) -> ArrayString<{ 2 * N }> {
      |                                         ^^^^^^^^^ required by this bound in `Hash::<N>::to_hex`
  ```
