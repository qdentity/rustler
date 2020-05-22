#![feature(link_args)]
#![link_args = "-rdynamic"]

use rustler_mock_rt::{Env, Atom};
use rustler_mock_rt::harness::NifLib;

//#[no_mangle]
//pub unsafe extern "C" fn enif_realloc_binary() {}

rustler_mock_rt::reexport_symbols!();

use std::path::Path;

fn main() {
    let lib = NifLib::load(Path::new("../rustler_tests/_build/test/lib/rustler_test/native/rustler_test/debug/librustler_test.so")).unwrap();

    let env = Env::new();
    let test_term = env.atom(Atom::new("ok"));

    let fun = lib.get_fun("atom_equals_ok", 1).unwrap();
    let ret = fun.call(&env, &[test_term.clone()]);

    assert!(ret == Atom::new("true"));
}
