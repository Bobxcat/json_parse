use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash, Hasher},
    sync::Arc,
};

use fxhash::{FxBuildHasher, FxHashMap};
use slotmap::{new_key_type, SlotMap};
use string_interner::DefaultHashBuilder;

use crate::parse::ParseErrTy;

type HashBuilder = FxBuildHasher;

new_key_type! {
    /// A symbol representing a unique entry in a `StrIntern` instance
    pub struct Sym;
}

#[derive(Debug, Clone)]
pub struct Entry {
    resolved: Arc<str>,
    hash: u64,
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.resolved == other.resolved
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
            resolved: Arc::from(s),
            hash: raw.hash,
        })
    }
}

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

#[derive(Debug, Clone)]
pub struct StrIntern {
    resolve: SlotMap<Sym, Entry>,
    raw: nohash::IntMap<RawEntry, (Sym, Entry)>,
}

impl StrIntern {
    pub fn new() -> Self {
        Self {
            resolve: SlotMap::default(),
            raw: HashMap::default(),
        }
    }

    pub fn get(&mut self, s_bytes: Vec<u8>) -> Result<Sym, ParseErrTy> {
        let raw = RawEntry::new(s_bytes);
        if let Some((sym, _)) = self.raw.get(&raw) {
            return Ok(*sym);
        }

        let entry = Entry::new(&raw)?;
        let sym = self.resolve.insert(entry.clone());
        self.raw.insert(raw, (sym, entry));
        Ok(sym)
    }

    pub fn resolve(&self, s: Sym) -> &str {
        &self.resolve[s].resolved
    }
}
