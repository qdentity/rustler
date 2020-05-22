use std::collections::HashMap;
use std::path::Path;
use std::error::Error;
use std::sync::Arc;
use std::ops::Deref;

use crate::{Env, Term, Atom};
use crate::string_or_bin::StringOrBin;

use std::os::raw::{c_void, c_int};
use rustler_sys::{ErlNifEnv, ERL_NIF_TERM, ErlNifEntry};

type NIF_INIT_FUN = unsafe extern "C" fn() -> *const ErlNifEntry;
type NIF_FUN = unsafe extern "C" fn(*mut ErlNifEnv, c_int, *const ERL_NIF_TERM) -> ERL_NIF_TERM;

#[derive(Debug, Clone)]
pub struct LibMarker(crate::arc::Arc<()>);
impl LibMarker {
    pub fn new() -> Self {
        LibMarker(crate::arc::Arc::new(()))
    }
}
impl PartialEq for LibMarker {
    fn eq(&self, other: &LibMarker) -> bool {
        self.0.same(&other.0)
    }
}

#[derive(Debug, Clone)]
pub struct NifLib(Arc<NifLibInner>);

#[derive(Debug)]
struct NifLibInner {
    lib: libloading::Library,
    entry: *const ErlNifEntry,
    functions: HashMap<(StringOrBin, usize), NifFun>,
    lib_id: LibMarker,
}

impl NifLib {

    pub fn load(path: &Path) -> Result<Self, Box<dyn Error>> {
        let lib = libloading::Library::new(path)?;
        let nif_init: libloading::Symbol<NIF_INIT_FUN> = unsafe { lib.get(b"nif_init") }?;

        let lib_id = LibMarker::new();

        let entry_ptr = unsafe { nif_init() };
        let entry = unsafe { &*entry_ptr };

        let funcs_slice = unsafe { std::slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        let functions = funcs_slice.iter().map(|fun_ptr| {
            let fun = unsafe { &*fun_ptr };

            let name_len = unsafe { libc::strlen(fun.name as *const i8) };
            let name_slice = unsafe { std::slice::from_raw_parts(fun.name, name_len) };
            let name = StringOrBin::from_bytes(name_slice);

            let arity = fun.arity as usize;

            let fun_struct = NifFunInner {
                name: name.clone(),
                arity,
                function: fun.function,
                flags: fun.flags,
                lib_id: lib_id.clone(),
            };
            let fun = NifFun(Arc::new(fun_struct));

            ((name, arity), fun)
        }).collect();

        let lib = NifLib(Arc::new(NifLibInner {
            lib,
            entry,
            functions,
            lib_id,
        }));

        Ok(lib)
    }

    pub fn get_fun<N: Into<StringOrBin>>(&self, name: N, arity: usize) -> Option<NifFun> {
        let name = name.into();
        self.0.functions.get(&(name, arity)).cloned()
    }

}

#[derive(Debug, Clone)]
pub struct NifFun(Arc<NifFunInner>);

impl Deref for NifFun {
    type Target = NifFunInner;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

#[derive(Debug)]
pub struct NifFunInner {
    pub name: StringOrBin,
    pub arity: usize,
    pub function: NIF_FUN,
    pub flags: u32,

    pub lib_id: LibMarker,
}

#[derive(Debug)]
pub enum NifReturn {
    Term(Term),
}

impl PartialEq<Atom> for NifReturn {
    fn eq(&self, rhs: &Atom) -> bool {
        match self {
            NifReturn::Term(term) => term == rhs,
        }
    }
}

impl NifFun {

    pub fn call(&self, env: &Env, args: &[Term]) -> NifReturn {
        let packed_env = env.pack();
        let packed_args: Vec<_> = args.iter().map(|t| t.pack()).collect();

        assert!(args.len() == self.arity);

        let ret = unsafe {
            (self.function)(
                packed_env as _,
                packed_args.len() as _,
                packed_args.as_ptr(),
            )
        };

        let ret_term = unsafe { Term::unpack(ret) };
        NifReturn::Term(ret_term)
    }

}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use crate::{Atom, Env};
    use super::NifLib;

    #[test]
    fn load() {
        let blah = &crate::nif_api::enif_realloc_binary;
        let lib = NifLib::load(Path::new("../target/debug/librustler_test.so")).unwrap();
        //println!("{:#?}", lib);
        let fun = lib.get_fun("atom_equals_ok", 1).unwrap();

        let env = Env::new();
        let test_term = env.atom(Atom::new("ok"));
        fun.call(&env, &[test_term]);

        println!("{:?}", fun);
    }

}
