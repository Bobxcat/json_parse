use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash},
    sync::Arc,
};

use fxhash::FxHashMap;
use slotmap::{new_key_type, SlotMap};
use string_interner::DefaultHashBuilder;

use crate::parse::ParseErrTy;

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
    fn new(bytes: &[u8]) -> Result<Self, ParseErrTy> {
        let s = std::str::from_utf8(bytes).map_err(|e| ParseErrTy::MalformedUtf8 {
            unexpected: bytes[e.valid_up_to()],
            at_offset: e.valid_up_to(),
        })?;
        let hash = DefaultHashBuilder::default().hash_one(&s);
        // <String as >
        Ok(Self {
            resolved: Arc::from(s),
            hash,
        })
    }
}

#[derive(Debug, Clone)]
pub struct StrIntern {
    resolve: SlotMap<Sym, Entry>,
    raw: FxHashMap<Vec<u8>, (Sym, Entry)>,
}

impl StrIntern {
    pub fn new() -> Self {
        Self {
            resolve: SlotMap::default(),
            raw: FxHashMap::default(),
        }
    }

    pub fn get(&mut self, s_bytes: Vec<u8>) -> Result<Sym, ParseErrTy> {
        if let Some((sym, _)) = self.raw.get(&s_bytes) {
            return Ok(*sym);
        }

        let entry = Entry::new(&s_bytes)?;
        let sym = self.resolve.insert(entry.clone());
        self.raw.insert(s_bytes, (sym, entry));
        Ok(sym)
    }

    pub fn resolve(&self, s: Sym) -> &str {
        &self.resolve[s].resolved
    }

    pub fn resolve_entry(&self, s: Sym) -> Entry {
        self.resolve[s].clone()
    }
}
