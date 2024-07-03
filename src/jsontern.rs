use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash, Hasher},
};

use fxhash::FxBuildHasher;

use crate::parse::ParseErrTy;

type HashBuilder = FxBuildHasher;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Sym(usize);

impl Hash for Sym {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0);
    }
}

impl nohash::IsEnabled for Sym {}

/// A view into [RawEntry] which views the bytes through a `*const str`.
///
/// It is a safety violation for an instance of this struct to live through
/// any mutation of the `RawEntry` it references
#[derive(Debug)]
struct Entry {
    resolved: *const str,
    hash: u64,
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.as_str() == other.as_str()
    }
}

impl Hash for Entry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Entry {
    fn new(raw: &RawEntry) -> Result<Self, ParseErrTy> {
        // Doing a clone is more or less unavoidable, but we can clone only once (when creating a new entry)
        let s = std::str::from_utf8(&raw.bytes).map_err(|e| ParseErrTy::MalformedUtf8 {
            unexpected: raw.bytes[e.valid_up_to()],
            at_offset: e.valid_up_to(),
        })?;
        Ok(Self {
            resolved: s as *const str,
            hash: raw.hash,
        })
    }

    fn clone_shallow(&self) -> Self {
        Self {
            resolved: self.resolved,
            hash: self.hash,
        }
    }

    fn as_str(&self) -> &str {
        // SAFETY: Since its creation, we have done no mutations to the owning `RawEntry`
        unsafe { &*self.resolved }
    }
}

/// See [Entry] for possible safety problems
#[derive(Debug, Clone)]
struct RawEntry {
    bytes: Vec<u8>,
    hash: u64,
}

impl RawEntry {
    fn new(bytes: Vec<u8>) -> Self {
        let hash = HashBuilder::default().hash_one(&bytes);
        Self { bytes, hash }
    }
}

impl PartialEq for RawEntry {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.bytes == other.bytes
    }
}

impl Eq for RawEntry {}

impl Hash for RawEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Only hash the precomputed hash to avoid extra computation,
        // and to enable nohash::IsEnabled
        state.write_u64(self.hash);
    }
}

impl nohash::IsEnabled for RawEntry {}

#[derive(Debug)]
pub struct StrIntern {
    sym_counter: usize,
    resolve: Vec<Entry>,
    raw: nohash::IntMap<RawEntry, (Sym, Entry)>,
}

impl StrIntern {
    pub fn new() -> Self {
        Self {
            sym_counter: 0,
            resolve: Vec::default(),
            raw: HashMap::default(),
        }
    }

    fn make_sym(&mut self) -> Sym {
        let s = Sym(self.sym_counter);
        self.sym_counter += 1;
        s
    }

    pub fn get(&mut self, s_bytes: Vec<u8>) -> Result<Sym, ParseErrTy> {
        let raw = RawEntry::new(s_bytes);
        if let Some((sym, _)) = self.raw.get(&raw) {
            return Ok(*sym);
        }

        let entry = Entry::new(&raw)?;
        let sym = self.make_sym();
        self.resolve.push(entry.clone_shallow());
        self.raw.insert(raw, (sym, entry));
        Ok(sym)
    }

    pub fn resolve(&self, s: Sym) -> &str {
        self.resolve[s.0].as_str()
    }
}

impl Clone for StrIntern {
    fn clone(&self) -> Self {
        // Note: We need to maintain the mapping of `Sym`s to `Entry`s
        // while cloning the data and making new `Entry`s pointing into new `RawEntry`s

        let mut entries = vec![];
        for (raw_entry, (sym, _do_not_use)) in &self.raw {
            let raw_entry = raw_entry.clone();
            let entry = Entry::new(&raw_entry).unwrap();
            entries.push((*sym, raw_entry, entry));
        }

        entries.sort_by_key(|(sym, _, _)| sym.0);
        let mut new = Self::new();

        for (sym, raw_entry, entry) in entries {
            new.raw.insert(raw_entry, (sym, entry.clone_shallow()));
            new.resolve.push(entry);
        }
        new.sym_counter = new.resolve.len();

        new
    }
}
