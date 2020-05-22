use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::collections::HashMap;
use std::sync::RwLock;

use crate::string_or_bin::StringOrBin;

lazy_static::lazy_static! {
    static ref ATOM_TABLE: RwLock<AtomTable> = {
        RwLock::new(AtomTable::new())
    };
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Atom(pub usize);

impl Atom {

    pub fn new(string: &str) -> Self {
        Atom::new_bytes(string.as_bytes())
    }

    pub fn new_bytes(bin: &[u8]) -> Self {
        let sob = StringOrBin::from_bytes(bin);
        if let Some(atom) = ATOM_TABLE.read().unwrap().atoms_back.get(&sob) {
           return *atom;
        }

        let mut wr = ATOM_TABLE.write().unwrap();
        let atom = Atom(wr.atoms.len());
        wr.atoms.push(sob.clone());
        wr.atoms_back.insert(sob, atom);
        atom
    }

    pub fn new_existing(string: &str) -> Option<Self> {
        Atom::new_bytes_existing(string.as_bytes())
    }

    pub fn new_bytes_existing(bin: &[u8]) -> Option<Self> {
        let sob = StringOrBin::from_bytes(bin);
        ATOM_TABLE.read().unwrap().atoms_back.get(&sob).cloned()
    }

}

impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let data = &ATOM_TABLE.read().unwrap().atoms[self.0];
        write!(f, "{:?}", data)
    }
}

type RawAtom = StringOrBin;

struct AtomTable {
    atoms: Vec<RawAtom>,
    atoms_back: HashMap<RawAtom, Atom>,
}

impl AtomTable {

    pub fn new() -> Self {
        AtomTable {
            atoms: Vec::new(),
            atoms_back: HashMap::new(),
        }
    }

}
