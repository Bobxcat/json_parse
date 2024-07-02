use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::{Debug, Display},
    io::{self, Read, Write},
    num::NonZeroUsize,
    slice,
    sync::Arc,
};

use ahash::HashMapExt;
use fxhash::FxHashMap;
use ptree::TreeItem;
use slotmap::{new_key_type, SlotMap};
use string_interner::{DefaultStringInterner, DefaultSymbol};
use ustr::{Ustr, UstrMap};

new_key_type! {
    pub struct ItemId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Sign {
    Pos,
    Neg,
}

#[derive(Debug, Clone)]
struct JsonDigits {
    digits: Vec<u8>,
}

impl JsonDigits {
    pub fn empty() -> Self {
        Self::from_digits(Vec::new())
    }
    pub fn from_digits(digits: Vec<u8>) -> Self {
        Self { digits }
    }
    pub fn push(&mut self, digit: u8) {
        self.digits.push(digit)
    }
    pub fn len(&self) -> usize {
        self.digits.len()
    }
    pub fn iter(&self) -> impl Iterator<Item = u8> {
        self.digits.clone().into_iter()
    }
    /// Calls `self.iter` and maps every digit to its corresponding ASCII character
    pub fn iter_chars(&self) -> impl Iterator<Item = char> {
        self.iter().map(|c| char::from_digit(c as u32, 10).unwrap())
    }
}

#[derive(Debug, Clone)]
struct JsonNum {
    sign: Sign,
    digits: JsonDigits,
    fraction: JsonDigits,
    exponent_sign: Sign,
    exponent: JsonDigits,
}

impl JsonNum {
    pub fn zero(sign: Sign) -> Self {
        Self {
            sign,
            digits: JsonDigits::from_digits(vec![0]),
            fraction: JsonDigits::empty(),
            exponent_sign: Sign::Pos,
            exponent: JsonDigits::empty(),
        }
    }
}

impl std::fmt::Display for JsonNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        if self.sign == Sign::Neg {
            s.push('-');
        }

        for c in self.digits.iter_chars() {
            s.push(c);
        }

        if self.fraction.len() > 0 {
            s.push('.');
            for c in self.fraction.iter_chars() {
                s.push(c);
            }
        }

        if self.exponent.len() > 0 {
            if self.exponent_sign == Sign::Pos {
                s.push('+');
            } else {
                s.push('-');
            }
            for c in self.exponent.iter_chars() {
                s.push(c);
            }
        }

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
enum Item {
    Object {
        fields: FxHashMap<DefaultSymbol, ItemId>,
    },
    Array {
        elems: Vec<ItemId>,
    },
    String {
        s: DefaultSymbol,
    },
    Number {
        n: JsonNum,
    },
    Boolean {
        b: bool,
    },
    Null,
}

#[derive(Debug, Clone)]
struct CachedConsts {
    pub item_true: ItemId,
    pub item_false: ItemId,
    pub item_null: ItemId,
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    ty: Box<ParseErrTy>,
}

impl<E> From<E> for ParseErr
where
    ParseErrTy: From<E>,
{
    fn from(value: E) -> Self {
        Self {
            ty: Box::new(value.into()),
        }
    }
}

impl Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.ty, f)
    }
}

impl Error for ParseErr {}

#[derive(Debug, Clone)]
pub enum ParseErrTy {
    MalformedUtf8,
}

impl Display for ParseErrTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR TODO")
    }
}
pub struct JsonParser<I> {
    head: Option<ItemId>,
    str_intern: DefaultStringInterner,
    items: SlotMap<ItemId, Item>,
    cache: CachedConsts,
    cursor: ParseCursor<I>,
}

impl<I> std::fmt::Debug for JsonParser<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.items.iter()).finish()
    }
}
impl<I: ParseInput> JsonParser<I> {
    pub fn new(input: I) -> Self {
        let mut items = SlotMap::default();
        let cache = CachedConsts {
            item_true: items.insert(Item::Boolean { b: true }),
            item_false: items.insert(Item::Boolean { b: false }),
            item_null: items.insert(Item::Null),
        };
        Self {
            head: None,
            str_intern: DefaultStringInterner::new(),
            items,
            cache,
            cursor: ParseCursor::new(input),
        }
    }

    fn insert_true(&mut self) -> ItemId {
        self.cache.item_true
    }

    fn insert_false(&mut self) -> ItemId {
        self.cache.item_false
    }

    fn insert_null(&mut self) -> ItemId {
        self.cache.item_null
    }

    pub fn parse(&mut self) -> ItemId {
        let head = self.parse_any_item();
        self.head = Some(head);
        head
    }
    /// Parses the given string as a json value
    ///
    /// Skips whitespace before *and* after the item itself
    ///
    /// Every parse function leaves the cursor at the byte after the parsed item
    #[must_use]
    fn parse_any_item(&mut self) -> ItemId {
        self.cursor.skip_whitespace();

        let item = match self.cursor.get() {
            b'{' => self.parse_object(),
            b'[' => self.parse_array(),
            b'"' => self.parse_string(),
            b't' => self.parse_keyword_literal("true", Self::insert_true),
            b'f' => self.parse_keyword_literal("false", Self::insert_false),
            b'n' => self.parse_keyword_literal("null", Self::insert_null),
            n if byte_is_number_start(n) => self.parse_number(),
            _ => todo!(
                "[{}]Invalid start to a generic JSON Item: {:?}",
                self.cursor.bytes_read(),
                self.cursor.get() as char
            ),
        };
        self.cursor.skip_whitespace();
        item
    }

    #[inline(always)]
    #[must_use]
    fn parse_keyword_literal(
        &mut self,
        literal_str: &'static str,
        insert: impl FnOnce(&mut Self) -> ItemId,
    ) -> ItemId {
        for &b in literal_str.as_bytes() {
            assert_eq!(
                self.cursor.get(),
                b,
                "[{}]Could not parse `{literal_str}`, encountered unexpected character: {:?}",
                self.cursor.bytes_read(),
                self.cursor.get() as char
            );
            self.cursor.step()
        }
        assert!(
            !self.cursor.get().is_ascii_digit(),
            "[{}]Could not parse `{literal_str}`, expected whitespace or delimiter but found: {:?}",
            self.cursor.bytes_read(),
            self.cursor.get() as char
        );
        // self.items.insert(item)
        (insert)(self)
    }

    #[must_use]
    fn parse_object(&mut self) -> ItemId {
        assert_eq!(self.cursor.get(), b'{');
        self.cursor.step();
        self.cursor.skip_whitespace();

        let mut fields = FxHashMap::new();

        loop {
            // In case of empty object, check at the start
            if self.cursor.get() == b'}' {
                break;
            }
            self.cursor.skip_whitespace();

            let name = self.parse_string();
            let name = match &self.items[name] {
                Item::String { s } => *s,
                _ => unreachable!(),
            };
            self.cursor.skip_whitespace();
            let delim = self.cursor.get();
            assert_eq!(
                delim,
                b':',
                "[{}]Error parsing `Object` Item, encountered {:?} instead of ':'",
                self.cursor.bytes_read(),
                delim as char,
            );
            self.cursor.step();

            let item = self.parse_any_item();
            fields.insert(name, item);

            match self.cursor.get() {
                b'}' => break,
                b',' => {
                    self.cursor.step();
                    continue;
                }
                c => panic!(
                    "[{}]Error parsing `Object` Item, encountered {:?} instead of '}}' or ','",
                    self.cursor.bytes_read(),
                    c as char
                ),
            }
        }

        assert_eq!(self.cursor.get(), b'}');
        self.cursor.step();
        self.items.insert(Item::Object { fields })
    }

    #[must_use]
    fn parse_array(&mut self) -> ItemId {
        assert_eq!(self.cursor.get(), b'[');

        self.cursor.step();
        self.cursor.skip_whitespace();
        let mut elems = vec![];
        loop {
            // In case of empty array, check at the start
            if self.cursor.get() == b']' {
                break;
            }

            let item = self.parse_any_item();
            elems.push(item);

            match self.cursor.get() {
                b']' => break,
                b',' => {
                    self.cursor.step();
                    continue;
                }
                c => panic!(
                    "[{}]Error parsing `Array` Item, encountered {:?} instead of ']' or ','",
                    self.cursor.bytes_read(),
                    c as char
                ),
            }
        }

        assert_eq!(self.cursor.get(), b']');
        self.cursor.step();
        self.items.insert(Item::Array { elems })
    }
    /// Parses expecting a string, putting the cursor at the byte after the end quote of the string
    #[must_use]
    fn parse_string(&mut self) -> ItemId {
        let mut s_bytes = vec![];
        assert_eq!(
            self.cursor.get(),
            b'"',
            "[{}]Error parsing `string` Item, encountered {:?} instead of '\"'",
            self.cursor.bytes_read(),
            self.cursor.get() as char
        );

        self.cursor.step();
        loop {
            match self.cursor.get() {
                b'\\' => {
                    let escaped = self.parse_string_escape();
                    s_bytes.extend_from_slice(escaped.as_slice());
                }
                b'"' => {
                    break;
                }
                b => {
                    s_bytes.push(b);
                    self.cursor.step();
                }
            }
        }

        debug_assert_eq!(self.cursor.get(), b'"');
        assert!(
            !self.cursor.finished(),
            "Parsing string value failed: string did not terminate before End of Input"
        );

        self.cursor.step();

        let s = std::str::from_utf8(&s_bytes).expect("Malformed UTF-8 in string!");
        let s = self.str_intern.get_or_intern(s);
        self.items.insert(Item::String { s })
    }
    /// Parses a JSON string escape sequence, such as `\n` or `\/` or `\u2122`
    ///
    /// Leaves the cursor at the first byte *after* the escaped sequence
    #[inline]
    #[must_use]
    fn parse_string_escape(&mut self) -> StringEscapeBytes {
        assert_eq!(self.cursor.get(), b'\\');
        self.cursor.step();
        match self.cursor.get() {
            single_char if [b'"', b'\\', b'/'].contains(&single_char) => {
                self.cursor.step();
                single_char.into()
            }
            b'b' => {
                self.cursor.step();
                0x8.into()
            }
            b'f' => {
                self.cursor.step();
                0xC.into()
            }
            b'n' => {
                self.cursor.step();
                b'\n'.into()
            }
            b'r' => {
                self.cursor.step();
                0xD.into()
            }
            b't' => {
                self.cursor.step();
                0x9.into()
            }
            b'u' => {
                self.cursor.step();
                // The values of each digit, in the range 0..16
                let mut dig = [0u16; 4];
                for i in 0..4 {
                    dig[i] = ascii_hex_digit_to_u8(self.cursor.get()) as u16;
                    self.cursor.step();
                }

                // Stolen directly from https://docs.rs/json/latest/src/json/parser.rs.html#441
                let encoded: u16 = dig[0] << 12 | dig[1] << 8 | dig[2] << 4 | dig[3];

                let uni = char::try_from(encoded as u32)
                    .expect(&format!("Bad Unicode Codepoint: \\u{:?}", dig));

                uni.into()
            }
            other => panic!(
                "[{}]Invalid `string` escape sequence: no sequence starts with \\{:?}",
                self.cursor.bytes_read(),
                other as char
            ),
        }
    }

    #[must_use]
    fn parse_number(&mut self) -> ItemId {
        assert!(byte_is_number_start(self.cursor.get()));
        let sign;
        if self.cursor.get() == b'-' {
            sign = Sign::Neg;
            self.cursor.step();
        } else {
            sign = Sign::Pos;
        }

        let mut digits = JsonDigits::empty();

        if self.cursor.get() == b'0' {
            self.cursor.step();
            digits.push(0);
        } else {
            self.cursor.go_until_for_each(
                |ascii| !ascii.is_ascii_digit(),
                |ascii| digits.push(ascii_digit_to_u8(ascii)),
            );
        }

        let mut fraction = JsonDigits::empty();
        if self.cursor.get() == b'.' {
            self.cursor.step();
            self.cursor.go_until_for_each(
                |ascii| !ascii.is_ascii_digit(),
                |ascii| fraction.push(ascii_digit_to_u8(ascii)),
            );
        }

        let mut exponent_sign = Sign::Pos;
        let mut exponent = JsonDigits::empty();
        if self.cursor.get() == b'E' || self.cursor.get() == b'e' {
            self.cursor.step();
            let sign;
            if self.cursor.get() == b'-' {
                self.cursor.step();
                sign = Sign::Neg;
            } else if self.cursor.get() == b'+' {
                self.cursor.step();
                sign = Sign::Pos;
            } else {
                sign = Sign::Pos;
            }
            exponent_sign = sign;

            self.cursor.go_until_for_each(
                |ascii| !ascii.is_ascii_digit(),
                |ascii| exponent.push(ascii_digit_to_u8(ascii)),
            );
        }

        let n = JsonNum {
            sign,
            digits,
            fraction,
            exponent_sign,
            exponent,
        };
        self.items.insert(Item::Number { n })
    }
}

/// A value representing a parsed unicode escape sequence, one to four bytes long
#[derive(Debug, Clone, Copy)]
struct StringEscapeBytes {
    bytes: [u8; 4],
    len: usize,
}

impl StringEscapeBytes {
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        &self.bytes[..self.len]
    }
}

impl From<u8> for StringEscapeBytes {
    #[inline(always)]
    fn from(value: u8) -> Self {
        Self {
            bytes: [value, 0, 0, 0],
            len: 1,
        }
    }
}

impl From<char> for StringEscapeBytes {
    #[inline(always)]
    fn from(value: char) -> Self {
        let value = value.to_string();
        let val = value.as_bytes();
        let bytes = std::array::from_fn(|idx| val.get(idx).copied().unwrap_or(0));
        Self {
            bytes,
            len: val.len(),
        }
    }
}

/// Input that can be used for parsing
pub trait ParseInput {
    /// Gets the next byte given the amount of bytes that have been read before this one
    /// (This is also the index of this byte).
    /// By no means does an implementation need to use `curr_read`
    fn next_byte(&mut self, curr_read: usize) -> Option<u8>;
}

impl ParseInput for Arc<[u8]> {
    #[inline(always)]
    fn next_byte(&mut self, curr_read: usize) -> Option<u8> {
        self.get(curr_read).copied()
    }
}

impl ParseInput for &[u8] {
    #[inline(always)]
    fn next_byte(&mut self, curr_read: usize) -> Option<u8> {
        self.get(curr_read).copied()
    }
}

impl ParseInput for VecDeque<u8> {
    #[inline(always)]
    fn next_byte(&mut self, _: usize) -> Option<u8> {
        self.pop_back()
    }
}

impl<R: Read> ParseInput for std::io::BufReader<R> {
    #[inline(always)]
    fn next_byte(&mut self, _: usize) -> Option<u8> {
        let mut b = [0];
        self.read_exact(&mut b).ok()?;
        Some(b[0])
    }
}

impl ParseInput for Box<dyn ParseInput> {
    #[inline(always)]
    fn next_byte(&mut self, r: usize) -> Option<u8> {
        (**self).next_byte(r)
    }
}

#[inline(always)]
fn byte_is_number_start(b: u8) -> bool {
    b == b'-' || b.is_ascii_digit()
}

/// Turns the input byte from an ASCII digit to the integer representation of that digit
#[inline(always)]
fn ascii_digit_to_u8(b: u8) -> u8 {
    match b {
        b'0' => 0,
        b'1' => 1,
        b'2' => 2,
        b'3' => 3,
        b'4' => 4,
        b'5' => 5,
        b'6' => 6,
        b'7' => 7,
        b'8' => 8,
        b'9' => 9,
        _ => panic!("Called `ascii_digit_to_u8` on `{:?}`", b as char),
    }
}

#[inline(always)]
fn ascii_hex_digit_to_u8(b: u8) -> u8 {
    match b.to_ascii_lowercase() {
        b'a' => 10,
        b'b' => 11,
        b'c' => 12,
        b'd' => 13,
        b'e' => 14,
        b'f' => 15,
        _ => ascii_digit_to_u8(b),
    }
}

/// A cursor which stores its position within the raw bytes it stores
///
/// The raw bytes are stored in an `Arc<[u8]>` for ease of use.
/// This does not cause much overhead since cursors are not cloned during parsing
#[derive(Debug)]
struct ParseCursor<I> {
    curr_char: u8,
    finished: bool,
    bytes_read: usize,
    input: I,
}

impl<I: ParseInput> ParseCursor<I> {
    pub fn new(mut input: I) -> Self {
        Self {
            curr_char: input
                .next_byte(0)
                .expect("ParseCursor needs at least one byte of input"),
            finished: false,
            bytes_read: 1,
            input,
        }
    }
    /// Steps the cursor forwards once
    #[inline(always)]
    pub fn step(&mut self) {
        self.curr_char = match self.input.next_byte(self.bytes_read) {
            Some(c) => c,
            None => {
                self.finished = true;
                return;
            }
        };
        self.bytes_read += 1;
    }
    /// `step`s `count` times
    #[inline(always)]
    pub fn advance(&mut self, count: usize) {
        for _ in 0..count {
            self.step();
        }
    }
    #[inline(always)]
    pub fn get(&self) -> u8 {
        self.curr_char
    }

    #[inline(always)]
    /// The number of bytes that have been read so far
    pub fn bytes_read(&self) -> NonZeroUsize {
        self.bytes_read
            .try_into()
            .expect("Somehow `ParseCursor` read more chars than `usize` could handle!")
    }

    #[inline(always)]
    pub fn finished(&self) -> bool {
        self.finished
    }
    /// Advances this cursor until `terminate` returns `true` or until reaching end of input
    ///
    /// The cursor will end up on the index of the byte that returned `true`.
    /// If the current byte returns `true`, the cursor will not move
    ///
    /// For every byte (in order) where `terminate` returns `false`, `for_each` is called on that byte
    ///
    /// This function is marked to always inline when possible, which should allow for niche optimizations
    #[inline(always)]
    pub fn go_until_for_each(
        &mut self,
        terminate: impl Fn(u8) -> bool,
        mut for_each: impl FnMut(u8),
    ) {
        loop {
            if self.finished() {
                return;
            }

            if terminate(self.get()) {
                return;
            }

            for_each(self.get());

            self.step();
        }
    }

    /// Advances this cursor until `terminate` returns true or until exceeding `raw.len()`
    ///
    /// The cursor will end up on the index of the byte that returned `true`.
    /// If the current byte returns `true`, the cursor will not move
    #[inline(always)]
    pub fn go_until(&mut self, terminate: impl Fn(u8) -> bool) {
        self.go_until_for_each(terminate, std::mem::drop)
    }
    pub fn skip_whitespace(&mut self) {
        self.go_until(|b| !(b as char).is_whitespace())
    }
}

/// An [Item] wrapper for pretty-printing the tree. This is a fairly expensive type
#[derive(Debug, Clone)]
struct ItemPTree {
    this_name: Option<DefaultSymbol>,
    this: ItemId,
    items: Arc<SlotMap<ItemId, Item>>,
    str_intern: Arc<DefaultStringInterner>,
}

impl TreeItem for ItemPTree {
    type Child = Self;

    fn write_self<W: std::io::Write>(&self, f: &mut W, _: &ptree::Style) -> std::io::Result<()> {
        let preface = match self.this_name.clone() {
            Some(s) => format!("\"{}\": ", self.str_intern.resolve(s).unwrap()),
            None => format!(""),
        };
        match &self.items[self.this] {
            Item::Object { fields } => write!(f, "{preface}Object({})", fields.len())?,
            Item::Array { elems } => write!(f, "{preface}Array({})", elems.len())?,
            Item::String { s } => write!(f, "{preface}{:?}", self.str_intern.resolve(*s).unwrap())?,
            Item::Number { n } => write!(f, "{preface}{}", n)?,
            Item::Boolean { b } => write!(f, "{preface}{}", b)?,
            Item::Null => write!(f, "{preface}null")?,
        };

        Ok(())
    }

    fn children(&self) -> std::borrow::Cow<[Self::Child]> {
        match &self.items[self.this] {
            Item::Object { fields } => fields
                .iter()
                .map(|(name, id)| Self {
                    this_name: Some(name.clone()),
                    this: *id,
                    items: self.items.clone(),
                    str_intern: self.str_intern.clone(),
                })
                .collect::<Vec<_>>()
                .into(),
            Item::Array { elems } => elems
                .iter()
                .map(|id| Self {
                    this_name: None,
                    this: *id,
                    items: self.items.clone(),
                    str_intern: self.str_intern.clone(),
                })
                .collect::<Vec<_>>()
                .into(),
            Item::String { .. } | Item::Number { .. } | Item::Boolean { .. } | Item::Null => {
                vec![].into()
            }
        }
    }
}

impl<I> std::fmt::Display for JsonParser<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let p = ItemPTree {
            this_name: None,
            this: self.head.unwrap(),
            items: Arc::new(self.items.clone()),
            str_intern: Arc::new(self.str_intern.clone()),
        };
        let mut msg = Vec::new();
        ptree::write_tree(&p, &mut msg).unwrap();
        let msg = String::from_utf8(msg).unwrap();
        write!(f, "{msg}")
    }
}
