#![cfg_attr(not(feature = "std"), no_std)]
#![feature(generic_const_exprs)]

use arrayvec::ArrayString;
use core::fmt;

/// A 256-bit (32-byte) `Hash`
pub type Hash256 = Hash<32>;

/// A 512-bit (64-byte) `Hash`
pub type Hash512 = Hash<64>;

/// A cryptographic hash, represented as a fixed-length array of bytes, which provides
/// constant-time equality checking
///
/// `Hash<N>` implements [`From`] and [`Into`] for `[u8; N]`, and it provides an
/// explicit [`as_bytes`] method returning `&[u8; N]`. However, byte arrays
/// and slices don't provide constant-time equality checking, which is often a
/// security requirement in software that handles private data. `Hash` doesn't
/// implement [`Deref`] or [`AsRef`], to avoid situations where a type
/// conversion happens implicitly and the constant-time property is
/// accidentally lost.
///
/// `Hash` provides the [`to_hex`] and [`from_hex`] methods for converting to
/// and from hexadecimal. It also implements [`Display`] and [`FromStr`].
///
/// [`From`]: https://doc.rust-lang.org/std/convert/trait.From.html
/// [`Into`]: https://doc.rust-lang.org/std/convert/trait.Into.html
/// [`as_bytes`]: #method.as_bytes
/// [`Deref`]: https://doc.rust-lang.org/stable/std/ops/trait.Deref.html
/// [`AsRef`]: https://doc.rust-lang.org/std/convert/trait.AsRef.html
/// [`to_hex`]: #method.to_hex
/// [`from_hex`]: #method.from_hex
/// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
/// [`FromStr`]: https://doc.rust-lang.org/std/str/trait.FromStr.html
#[derive(Clone, Copy, Hash)]
pub struct Hash<const N: usize>([u8; N]);

impl<const N: usize> Hash<N> {
    /// The raw bytes of the `Hash`. Note that byte arrays don't provide
    /// constant-time equality checking, so if  you need to compare hashes,
    /// prefer the `Hash` type.
    #[inline]
    pub fn as_bytes(&self) -> &[u8; N] {
        &self.0
    }

    /// Encode a `Hash` in lowercase hexadecimal.
    ///
    /// The returned [`ArrayString`] is a fixed size and doesn't allocate memory
    /// on the heap. Note that [`ArrayString`] doesn't provide constant-time
    /// equality checking, so if you need to compare hashes, prefer the `Hash`
    /// type.
    ///
    /// [`ArrayString`]: https://docs.rs/arrayvec/0.5.1/arrayvec/struct.ArrayString.html
    pub fn to_hex(&self) -> ArrayString<{ 2 * N }> {
        let mut s = ArrayString::new();
        let table = b"0123456789abcdef";
        for &b in self.0.iter() {
            s.push(table[(b >> 4) as usize] as char);
            s.push(table[(b & 0xf) as usize] as char);
        }
        s
    }

    /// Decode a `Hash` from hexadecimal. Both uppercase and lowercase ASCII
    /// bytes are supported.
    ///
    /// Any byte outside the ranges `'0'...'9'`, `'a'...'f'`, and `'A'...'F'`
    /// results in an error. An input length other than 64 also results in an
    /// error.
    ///
    /// Note that `Hash` also implements `FromStr`, so `Hash::from_hex("...")`
    /// is equivalent to `"...".parse()`.
    pub fn from_hex(hex: impl AsRef<[u8]>) -> Result<Self, HexError> {
        fn hex_val(byte: u8) -> Result<u8, HexError> {
            match byte {
                b'A'..=b'F' => Ok(byte - b'A' + 10),
                b'a'..=b'f' => Ok(byte - b'a' + 10),
                b'0'..=b'9' => Ok(byte - b'0'),
                _ => Err(HexError(HexErrorInner::InvalidByte(byte))),
            }
        }
        let hex_bytes: &[u8] = hex.as_ref();
        if hex_bytes.len() != N * 2 {
            return Err(HexError(HexErrorInner::InvalidLen(hex_bytes.len())));
        }
        let mut hash_bytes: [u8; N] = [0; N];
        for i in 0..N {
            hash_bytes[i] = 16 * hex_val(hex_bytes[2 * i])? + hex_val(hex_bytes[2 * i + 1])?;
        }
        Ok(Hash::from(hash_bytes))
    }
}

impl<const N: usize> From<[u8; N]> for Hash<N> {
    #[inline]
    fn from(bytes: [u8; N]) -> Self {
        Self(bytes)
    }
}

impl<const N: usize> From<Hash<N>> for [u8; N] {
    #[inline]
    fn from(hash: Hash<N>) -> Self {
        hash.0
    }
}

impl<const N: usize> core::str::FromStr for Hash<N> {
    type Err = HexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Hash::<N>::from_hex(s)
    }
}

/// This implementation is constant-time.
impl<const N: usize> PartialEq for Hash<N> {
    #[inline]
    fn eq(&self, other: &Hash<N>) -> bool {
        constant_time_eq::constant_time_eq_n(&self.0, &other.0)
    }
}

/// This implementation is constant-time.
impl<const N: usize> PartialEq<[u8; N]> for Hash<N> {
    #[inline]
    fn eq(&self, other: &[u8; N]) -> bool {
        constant_time_eq::constant_time_eq_n(&self.0, other)
    }
}

/// This implementation is constant-time if the target is N bytes long.
impl<const N: usize> PartialEq<[u8]> for Hash<N> {
    #[inline]
    fn eq(&self, other: &[u8]) -> bool {
        constant_time_eq::constant_time_eq(&self.0, other)
    }
}

impl<const N: usize> Eq for Hash<N> {}

impl<const N: usize> fmt::Display for Hash<N>
where
    ArrayString<{ 2 * N }>: Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Formatting field as `&str` to reduce code size since the `Display`
        // dynamic dispatch table for `&str` is likely needed elsewhere already,
        // but that for `ArrayString<[u8; 64]>` is not.
        let hex: ArrayString<{ 2 * N }> = self.to_hex();
        let hex: &str = hex.as_str();

        f.write_str(hex)
    }
}

impl<const N: usize> fmt::Debug for Hash<N>
where
    ArrayString<{ 2 * N }>: Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Formatting field as `&str` to reduce code size since the `Debug`
        // dynamic dispatch table for `&str` is likely needed elsewhere already,
        // but that for `ArrayString<[u8; 64]>` is not.
        let hex: ArrayString<{ 2 * N }> = self.to_hex();
        let hex: &str = hex.as_str();

        f.debug_tuple("Hash").field(&hex).finish()
    }
}

/// The error type for [`Hash::from_hex`].
///
/// The `.to_string()` representation of this error currently distinguishes between bad length
/// errors and bad character errors. This is to help with logging and debugging, but it isn't a
/// stable API detail, and it may change at any time.
#[derive(Clone, Debug)]
pub struct HexError(HexErrorInner);

#[derive(Clone, Debug)]
enum HexErrorInner {
    InvalidByte(u8),
    InvalidLen(usize),
}

impl fmt::Display for HexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            HexErrorInner::InvalidByte(byte) => {
                if byte < 128 {
                    write!(f, "invalid hex character: {:?}", byte as char)
                } else {
                    write!(f, "invalid hex character: 0x{:x}", byte)
                }
            }
            HexErrorInner::InvalidLen(len) => {
                write!(f, "expected 64 hex bytes, received {}", len)
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for HexError {}

#[cfg(feature = "serde")]
impl<const N: usize> serde::Serialize for Hash<N>
where
    ArrayString<{ 2 * N }>: Sized,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            serializer.serialize_str(&self.to_hex())
        } else {
            serializer.serialize_bytes(self.as_bytes())
        }
    }
}

#[cfg(feature = "serde")]
struct HashVisitor<const N: usize>;

#[cfg(feature = "serde")]
impl<'de, const N: usize> serde::de::Visitor<'de> for HashVisitor<N> {
    type Value = Hash<N>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{} bytes or a {}-character hex string", N, 2 * N)
    }

    fn visit_str<E>(self, value: &str) -> Result<Hash<N>, E>
    where
        E: serde::de::Error,
    {
        Hash::from_hex(value).map_err(|_| E::custom("foo"))
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Hash<N>, E>
    where
        E: serde::de::Error,
    {
        if let Ok(array) = <[u8; N]>::try_from(value) {
            Ok(array.into())
        } else {
            Err(E::custom("foo"))
        }
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Hash<N>, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        use serde::de::Error;
        let mut array = [0u8; N];
        for i in &mut array {
            let Some(byte) = seq.next_element::<u8>()? else {
                return Err(A::Error::custom("foo"));
            };
            *i = byte;
        }
        if seq.next_element::<u8>()?.is_some() {
            return Err(A::Error::custom("bar"));
        }
        Ok(array.into())
    }
}

#[cfg(feature = "serde")]
impl<'de, const N: usize> serde::Deserialize<'de> for Hash<N> {
    fn deserialize<D>(deserializer: D) -> Result<Hash<N>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            deserializer.deserialize_string(HashVisitor)
        } else {
            deserializer.deserialize_bytes(HashVisitor)
        }
    }
}
