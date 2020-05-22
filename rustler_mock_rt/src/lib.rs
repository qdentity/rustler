#![feature(c_variadic, link_args)]
#![link_args = "-rdynamic"]

use std::sync::Mutex;
use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::path::Path;
use std::error::Error;

#[macro_export]
pub mod nif_api;

mod arc;
use arc::Arc;

mod atom;
pub use atom::Atom;

#[cfg(feature = "harness")]
pub mod harness;

mod string_or_bin;

#[derive(Debug, Clone)]
pub struct Env(Arc<EnvInner>);

impl Env {

    pub fn new() -> Self {
        Env(Arc::new(EnvInner {
            terms: Mutex::new(HashSet::new()),
        }))
    }

    pub fn equal(&self, other: &Env) -> bool {
        self.0.same(&other.0)
    }

    /// Packs the Env into a usize.
    /// This will be valid as long as the underlying env is in scope.
    pub fn pack(&self) -> usize {
        self.0.pack()
    }

    /// Unpacks the Env from the usize and takes a reference to it.
    pub unsafe fn unpack(env: usize) -> Self {
        Env(Arc::unpack(env))
    }

    fn add_term(&self, term: Term) {
        self.0.terms.lock().unwrap().insert(term.into_term_by_ref());
    }

    pub fn atom(&self, atom: Atom) -> Term {
        let term = Term::new(self.clone(), TermKind::Atom(atom));
        self.add_term(term.clone());
        term
    }

}

#[derive(Debug)]
struct EnvInner {
    terms: Mutex<HashSet<TermByRef>>,
}

#[derive(Clone)]
pub struct TermByRef(Term);

impl Debug for TermByRef {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "TermByRef({})", self.0.pack())
    }
}

impl std::hash::Hash for TermByRef {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.0.pack().hash(hasher);
    }
}

impl PartialEq for TermByRef {
    fn eq(&self, other: &TermByRef) -> bool {
        (self.0).0.same(&(other.0).0)
    }
}
impl Eq for TermByRef {}

#[derive(Debug, Clone)]
pub struct Term(Arc<TermInner>);

impl Term {

    pub fn new(env: Env, kind: TermKind) -> Self {
        Term(Arc::new(TermInner {
            env: Some(env),
            kind,
        }))
    }

    pub fn new_noenv(kind: TermKind) -> Self {
        Term(Arc::new(TermInner {
            env: None,
            kind,
        }))
    }

    pub fn into_term_by_ref(self) -> TermByRef {
        TermByRef(self)
    }

    pub fn is_in(&self, env: &Env) -> bool {
        if let Some(ienv) = &self.0.env {
            ienv.equal(env)
        } else {
            true
        }
    }

    pub fn kind(&self) -> &TermKind {
        &self.0.kind
    }

    /// Packs the Term into a usize.
    /// This will be valid as long as the underlying env is in scope.
    pub fn pack(&self) -> usize {
        match self.kind() {
            TermKind::Atom(atom) => {
                0b1 | (atom.0 << 1)
            },
            _ => {
                let packed = self.0.pack();
                assert!(packed & 0b11 == 0);
                packed
            },
        }
    }

    /// Unpacks the Term from the usize and takes a reference to it.
    pub unsafe fn unpack(term: usize) -> Self {
        if term & 0b1 == 1 {
            let atom = Atom(term >> 1);
            Term::new_noenv(TermKind::Atom(atom))
        } else {
            Term(Arc::unpack(term))
        }
    }

}

impl PartialEq<Atom> for Term {
    fn eq(&self, rhs: &Atom) -> bool {
        match self.kind() {
            TermKind::Atom(atom) => atom == rhs,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct TermInner {
    env: Option<Env>,
    kind: TermKind,
}

#[derive(Debug)]
pub enum TermKind {
    Float(f64),
    SmallInteger(i64),
    Atom(Atom),
}

macro_rules! impl_integer_tryfrom_term {
    ($typ:ty) => {
        impl TryFrom<&Term> for $typ {
            type Error = ();
            fn try_from(term: &Term) -> Result<$typ, ()> {
                match term.kind() {
                    TermKind::SmallInteger(int) => (*int).try_into().map_err(|_| ()),
                    _ => Err(()),
                }
            }
        }
    };
}

impl_integer_tryfrom_term!(i32);
impl_integer_tryfrom_term!(u32);
impl_integer_tryfrom_term!(i64);
impl_integer_tryfrom_term!(u64);
