#![allow(non_camel_case_types)]

pub use rustler_sys::{
    ERL_NIF_TERM, ErlNifEnv,
    ErlNifMonitor, ErlNifPid, ErlNifTermType,
    ErlNifPort, ErlNifHash, ErlNifResourceType,
    ErlNifResourceFlags, ErlNifResourceTypeInit,
    ErlNifSelectFlags, ErlNifEvent, ErlNifBinaryToTerm,
    ErlNifBinary, ErlNifUniqueInteger, ErlNifTime,
    ErlNifTimeUnit, ErlNifMapIterator, ErlNifMapIteratorEntry,
    ErlNifCharEncoding, ErlNifSysInfo,
    enif_make_pid, enif_get_int64, enif_make_int64,
    enif_make_uint64, enif_get_uint64,
    ERL_NIF_THR_DIRTY_CPU_SCHEDULER,
    ERL_NIF_THR_DIRTY_IO_SCHEDULER,
    ERL_NIF_THR_NORMAL_SCHEDULER, ERL_NIF_THR_UNDEFINED,
};

use std::convert::TryInto;
use crate::{Env, Term, TermKind, Atom};

const FALSE: c_int = 1;
const TRUE: c_int = 1;

use std::os::raw::{c_void, c_int, c_uint, c_char, c_uchar, c_double, c_long, c_ulong};
type size_t = usize;

/// Because of https://github.com/rust-lang/rust/issues/50007, we need to
/// redefine all of these in any executable that loads a NIF.
///
/// Really damn annoying, but I guess it will have to do for now.
///
/// For some reason this issue is really random/inconsitent too.
/// Reexporting this one symbol seems to fix all the others as well (??).
#[macro_export]
macro_rules! reexport_symbols {
	() => {
        pub static enif_priv_data: unsafe extern "C" fn(*mut rustler_sys::rustler_sys_api::ErlNifEnv) -> *mut std::ffi::c_void = $crate::nif_api::enif_priv_data;
	};
}

#[no_mangle]
pub unsafe extern "C" fn enif_snprintf(out: *mut c_char, size: usize, format: *const c_char, ...) -> c_int {
    unimplemented!("_enif_snprintf")
}

#[macro_export]
macro_rules! enif_snprintf {
    ( $( $arg:expr ),*  ) => { $crate::_enif_snprintf($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_snprintf!($($arg),*) };
}

pub type NIF_TERM = ERL_NIF_TERM;
pub type NIF_ENV = *mut ErlNifEnv;

#[no_mangle]
pub unsafe extern "C" fn enif_priv_data(arg1: NIF_ENV) -> *mut c_void {
    unimplemented!("enif_priv_data")
}

#[no_mangle]
pub unsafe extern "C" fn enif_alloc(size: size_t) -> *mut c_void {
    unimplemented!("enif_alloc")
}

#[no_mangle]
pub unsafe extern "C" fn enif_free(ptr: *mut c_void) {
    unimplemented!("enif_free")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_atom(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    println!("CALL: enif_is_atom");

    let env = Env::unpack(arg1 as usize);
    let term = Term::unpack(term);
    assert!(term.is_in(&env));

    match term.kind() {
        TermKind::Atom(_) => TRUE,
        _ => FALSE,
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_binary(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_ref(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_ref")
}

#[no_mangle]
pub unsafe extern "C" fn enif_inspect_binary(arg1: NIF_ENV, bin_term: NIF_TERM, bin: *mut ErlNifBinary) -> c_int {
    unimplemented!("enif_inspect_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_alloc_binary(size: size_t, bin: *mut ErlNifBinary) -> c_int {
    unimplemented!("enif_alloc_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_realloc_binary(bin: *mut ErlNifBinary, size: size_t) -> c_int {
    unimplemented!("enif_realloc_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_release_binary(bin: *mut ErlNifBinary) {
    unimplemented!("enif_release_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_int(arg1: NIF_ENV, term: NIF_TERM, ip: *mut c_int) -> c_int {
    unimplemented!("enif_get_int");

    let env = Env::unpack(arg1 as usize);
    let term = Term::unpack(term);
    assert!(term.is_in(&env));

    if let Ok(int) = (&term).try_into() {
        *ip = int;
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_ulong(arg1: NIF_ENV, term: NIF_TERM, ip: *mut c_ulong) -> c_int {
    unimplemented!("enif_get_ulong");

    let env = Env::unpack(arg1 as usize);
    let term = Term::unpack(term);
    assert!(term.is_in(&env));

    if let Ok(int) = (&term).try_into() {
        *ip = int;
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_double(arg1: NIF_ENV, term: NIF_TERM, dp: *mut c_double) -> c_int {
    unimplemented!("enif_get_double")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_list_cell(env: NIF_ENV, term: NIF_TERM, head: *mut NIF_TERM, tail: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_get_list_cell")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_tuple(env: NIF_ENV, tpl: NIF_TERM, arity: *mut c_int, array: *mut *const NIF_TERM) -> c_int {
    unimplemented!("enif_get_tuple")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_identical(lhs: NIF_TERM, rhs: NIF_TERM) -> c_int {
    unimplemented!("enif_is_identical")
}

#[no_mangle]
pub unsafe extern "C" fn enif_compare(lhs: NIF_TERM, rhs: NIF_TERM) -> c_int {
    println!("CALL: enif_compare");
    unimplemented!("enif_compare")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_binary(env: NIF_ENV, bin: *mut ErlNifBinary) -> NIF_TERM {
    unimplemented!("enif_make_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_badarg(env: NIF_ENV) -> NIF_TERM {
    unimplemented!("enif_make_badarg")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_int(env: NIF_ENV, i: c_int) -> NIF_TERM {
    unimplemented!("enif_make_int")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_ulong(env: NIF_ENV, i: c_ulong) -> NIF_TERM {
    unimplemented!("enif_make_ulong")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_double(env: NIF_ENV, d: c_double) -> NIF_TERM {
    unimplemented!("enif_make_double")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_atom(env: NIF_ENV, name: *const c_uchar) -> NIF_TERM {
    unimplemented!("enif_make_atom");

    let env = Env::unpack(env as usize);

    let len = libc::strlen(name as *const i8);
    let slice = std::slice::from_raw_parts(name, len);

    let atom = Atom::new_bytes(slice);
    let term = env.atom(atom);
    term.pack()
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_existing_atom(env: NIF_ENV, name: *const c_uchar, atom: *mut NIF_TERM, _arg1: ErlNifCharEncoding) -> c_int {
    unimplemented!("enif_make_existing_atom");

    let env = Env::unpack(env as usize);

    let len = libc::strlen(name as *const i8);
    let slice = std::slice::from_raw_parts(name, len);

    if let Some(natom) = Atom::new_bytes_existing(slice) {
        let term = env.atom(natom);
        *atom = term.pack();
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_list_cell(env: NIF_ENV, car: NIF_TERM, cdr: NIF_TERM) -> NIF_TERM {
    unimplemented!("enif_make_list_cell")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_string(env: NIF_ENV, string: *const c_uchar, arg1: ErlNifCharEncoding) -> NIF_TERM {
    unimplemented!("enif_make_string")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_ref(env: NIF_ENV) -> NIF_TERM {
    unimplemented!("enif_make_ref")
}

#[no_mangle]
pub unsafe extern "C" fn enif_realloc(ptr: *mut c_void, size: size_t) -> *mut c_void {
    unimplemented!("enif_realloc")
}

#[no_mangle]
pub unsafe extern "C" fn enif_system_info(sip: *mut ErlNifSysInfo, si_size: size_t) {
    unimplemented!("enif_system_info")
}

#[no_mangle]
pub unsafe extern "C" fn enif_inspect_iolist_as_binary(arg1: NIF_ENV, term: NIF_TERM, bin: *mut ErlNifBinary) -> c_int {
    unimplemented!("enif_inspect_iolist_as_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_sub_binary(arg1: NIF_ENV, bin_term: NIF_TERM, pos: size_t, size: size_t) -> NIF_TERM {
    unimplemented!("enif_make_sub_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_string(arg1: NIF_ENV, list: NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding) -> c_int {
    unimplemented!("enif_get_string")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_atom(arg1: NIF_ENV, atom: NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding) -> c_int {
    unimplemented!("enif_get_atom")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_fun(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_fun")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_pid(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_pid")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_port(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_port")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_uint(arg1: NIF_ENV, term: NIF_TERM, ip: *mut c_uint) -> c_int {
    unimplemented!("enif_get_uint");

    let env = Env::unpack(arg1 as usize);
    let term = Term::unpack(term);
    assert!(term.is_in(&env));

    if let Ok(int) = (&term).try_into() {
        *ip = int;
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_long(arg1: NIF_ENV, term: NIF_TERM, ip: *mut c_long) -> c_int {
    unimplemented!("enif_get_long");

    let env = Env::unpack(arg1 as usize);
    let term = Term::unpack(term);
    assert!(term.is_in(&env));

    if let Ok(int) = (&term).try_into() {
        *ip = int;
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_uint(arg1: NIF_ENV, i: c_uint) -> NIF_TERM {
    unimplemented!("enif_make_uint")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_long(arg1: NIF_ENV, i: c_long) -> NIF_TERM {
    unimplemented!("enif_make_long")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_tuple_from_array(arg1: NIF_ENV, arr: *const NIF_TERM, cnt: c_uint) -> NIF_TERM {
    unimplemented!("enif_make_tuple_from_array")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_list_from_array(arg1: NIF_ENV, arr: *const NIF_TERM, cnt: c_uint) -> NIF_TERM {
    unimplemented!("enif_make_list_from_array")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_empty_list(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_empty_list")
}

#[no_mangle]
pub unsafe extern "C" fn enif_open_resource_type(arg1: NIF_ENV, module_str: *const c_uchar, name_str: *const c_uchar, dtor: Option<unsafe extern "C" fn (arg1: NIF_ENV, arg2: *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType {
    unimplemented!("enif_open_resource_type")
}

#[no_mangle]
pub unsafe extern "C" fn enif_alloc_resource(_type: *const ErlNifResourceType, size: size_t) -> *mut c_void {
    unimplemented!("enif_alloc_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_release_resource(obj: *const c_void) {
    unimplemented!("enif_release_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_resource(arg1: NIF_ENV, obj: *const c_void) -> NIF_TERM {
    unimplemented!("enif_make_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_resource(arg1: NIF_ENV, term: NIF_TERM, _type: *const ErlNifResourceType, objp: *mut *const c_void) -> c_int {
    unimplemented!("enif_get_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_sizeof_resource(obj: *const c_void) -> size_t {
    unimplemented!("enif_sizeof_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_new_binary(arg1: NIF_ENV, size: size_t, termp: *mut NIF_TERM) -> *mut c_uchar {
    unimplemented!("enif_make_new_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_list(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_list")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_tuple(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_tuple")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_atom_length(arg1: NIF_ENV, atom: NIF_TERM, len: *mut c_uint, arg2: ErlNifCharEncoding) -> c_int {
    unimplemented!("enif_get_atom_length")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_list_length(env: NIF_ENV, term: NIF_TERM, len: *mut c_uint) -> c_int {
    unimplemented!("enif_get_list_length")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_atom_len(env: NIF_ENV, name: *const c_uchar, len: size_t) -> NIF_TERM {
    println!("CALL: enif_make_atom_len");

    let env = Env::unpack(env as usize);

    let slice = std::slice::from_raw_parts(name, len);

    let atom = Atom::new_bytes(slice);
    let term = env.atom(atom);
    term.pack()
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_existing_atom_len(env: NIF_ENV, name: *const c_uchar, len: size_t, atom: *mut NIF_TERM, arg1: ErlNifCharEncoding) -> c_int {
    unimplemented!("enif_make_existing_atom_len");

    let env = Env::unpack(env as usize);

    let slice = std::slice::from_raw_parts(name, len);

    if let Some(natom) = Atom::new_bytes_existing(slice) {
        let term = env.atom(natom);
        *atom = term.pack();
        TRUE
    } else {
        FALSE
    }
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_string_len(env: NIF_ENV, string: *const c_uchar, len: size_t, arg1: ErlNifCharEncoding) -> NIF_TERM {
    unimplemented!("enif_make_string_len")
}

#[no_mangle]
pub unsafe extern "C" fn enif_alloc_env() -> NIF_ENV {
    println!("CALL: enif_alloc_env");

    let env = Env::new();
    let packed = env.pack();
    std::mem::forget(env);
    packed as _
}

#[no_mangle]
pub unsafe extern "C" fn enif_free_env(env: NIF_ENV) {
    println!("CALL: enif_free_env");

    let env = Env::unpack(env as usize);
    env.0.decrease_rc();
}

#[no_mangle]
pub unsafe extern "C" fn enif_clear_env(env: NIF_ENV) {
    unimplemented!("enif_clear_env")
}

#[no_mangle]
pub unsafe extern "C" fn enif_send(env: NIF_ENV, to_pid: *const ErlNifPid, msg_env: NIF_ENV, msg: NIF_TERM) -> c_int {
    unimplemented!("enif_send")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_copy(dst_env: NIF_ENV, src_term: NIF_TERM) -> NIF_TERM {
    unimplemented!("enif_make_copy")
}

#[no_mangle]
pub unsafe extern "C" fn enif_self(caller_env: NIF_ENV, pid: *mut ErlNifPid) -> *mut ErlNifPid {
    unimplemented!("enif_self")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_local_pid(env: NIF_ENV, arg1: NIF_TERM, pid: *mut ErlNifPid) -> c_int {
    unimplemented!("enif_get_local_pid")
}

#[no_mangle]
pub unsafe extern "C" fn enif_keep_resource(obj: *const c_void) {
    unimplemented!("enif_keep_resource")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_resource_binary(arg1: NIF_ENV, obj: *const c_void, data: *const c_void, size: size_t) -> NIF_TERM {
    unimplemented!("enif_make_resource_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_exception(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_exception")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_reverse_list(arg1: NIF_ENV, term: NIF_TERM, list: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_make_reverse_list")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_number(arg1: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_number")
}

#[no_mangle]
pub unsafe extern "C" fn enif_dlopen(lib: *const c_uchar, err_handler: Option<unsafe extern "C" fn (arg1: *mut c_void, arg2: *const c_uchar)>, err_arg: *mut c_void) -> *mut c_void {
    unimplemented!("enif_dlopen")
}

#[no_mangle]
pub unsafe extern "C" fn enif_dlsym(handle: *mut c_void, symbol: *const c_uchar, err_handler: Option<unsafe extern "C" fn (arg1: *mut c_void, arg2: *const c_uchar)>, err_arg: *mut c_void) -> *mut c_void {
    unimplemented!("enif_dlsym")
}

#[no_mangle]
pub unsafe extern "C" fn enif_consume_timeslice(arg1: NIF_ENV, percent: c_int) -> c_int {
    unimplemented!("enif_consume_timeslice")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_map(env: NIF_ENV, term: NIF_TERM) -> c_int {
    unimplemented!("enif_is_map")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_map_size(env: NIF_ENV, term: NIF_TERM, size: *mut size_t) -> c_int {
    unimplemented!("enif_get_map_size")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_new_map(env: NIF_ENV) -> NIF_TERM {
    unimplemented!("enif_make_new_map")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_map_put(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_make_map_put")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, value: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_get_map_value")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_map_update(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_make_map_update")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_map_remove(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_make_map_remove")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_create(env: NIF_ENV, map: NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry) -> c_int {
    unimplemented!("enif_map_iterator_create")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_destroy(env: NIF_ENV, iter: *mut ErlNifMapIterator) {
    unimplemented!("enif_map_iterator_destroy")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_is_head(env: NIF_ENV, iter: *mut ErlNifMapIterator) -> c_int {
    unimplemented!("enif_map_iterator_is_head")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_is_tail(env: NIF_ENV, iter: *mut ErlNifMapIterator) -> c_int {
    unimplemented!("enif_map_iterator_is_tail")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_next(env: NIF_ENV, iter: *mut ErlNifMapIterator) -> c_int {
    unimplemented!("enif_map_iterator_next")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_prev(env: NIF_ENV, iter: *mut ErlNifMapIterator) -> c_int {
    unimplemented!("enif_map_iterator_prev")
}

#[no_mangle]
pub unsafe extern "C" fn enif_map_iterator_get_pair(env: NIF_ENV, iter: *mut ErlNifMapIterator, key: *mut NIF_TERM, value: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_map_iterator_get_pair")
}

#[no_mangle]
pub unsafe extern "C" fn enif_schedule_nif(env: NIF_ENV, fun_name: *const c_uchar, flags:c_int, fp: unsafe extern "C" fn(env: NIF_ENV, argc:c_int, argv:*const NIF_TERM) -> NIF_TERM, argc:c_int, argv:*const NIF_TERM) -> NIF_TERM {
    unimplemented!("enif_schedule_nif")
}

#[no_mangle]
pub unsafe extern "C" fn enif_has_pending_exception(env: NIF_ENV, reason: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_has_pending_exception")
}

#[no_mangle]
pub unsafe extern "C" fn enif_raise_exception(env: NIF_ENV, reason: NIF_TERM) -> NIF_TERM {
    unimplemented!("enif_raise_exception")
}

#[no_mangle]
pub unsafe extern "C" fn enif_getenv(key: *const c_uchar, value: *mut c_uchar, value_size: *mut size_t) -> c_int {
    unimplemented!("enif_getenv")
}

#[no_mangle]
pub unsafe extern "C" fn enif_monotonic_time(unit: ErlNifTimeUnit) -> ErlNifTime {
    unimplemented!("enif_monotonic_time")
}

#[no_mangle]
pub unsafe extern "C" fn enif_time_offset(unit: ErlNifTimeUnit) -> ErlNifTime {
    unimplemented!("enif_time_offset")
}

#[no_mangle]
pub unsafe extern "C" fn enif_convert_time_unit(time: ErlNifTime, from_unit: ErlNifTimeUnit, to_unit: ErlNifTimeUnit) -> ErlNifTime {
    unimplemented!("enif_convert_time_unit")
}

#[no_mangle]
pub unsafe extern "C" fn enif_now_time(env: NIF_ENV) -> NIF_TERM {
    unimplemented!("enif_now_time")
}

#[no_mangle]
pub unsafe extern "C" fn enif_cpu_time(env: NIF_ENV) -> NIF_TERM {
    unimplemented!("enif_cpu_time")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_unique_integer(env: NIF_ENV, properties: ErlNifUniqueInteger) -> NIF_TERM {
    unimplemented!("enif_make_unique_integer")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_current_process_alive(env: NIF_ENV) -> c_int {
    unimplemented!("enif_is_current_process_alive")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_process_alive(env: NIF_ENV, pid: *const ErlNifPid) -> c_int {
    unimplemented!("enif_is_process_alive")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_port_alive(env: NIF_ENV, port_id: *const ErlNifPort) -> c_int {
    unimplemented!("enif_is_port_alive")
}

#[no_mangle]
pub unsafe extern "C" fn enif_get_local_port(env: NIF_ENV, term: NIF_TERM, port_id: *mut ErlNifPort) -> c_int {
    unimplemented!("enif_get_local_port")
}

#[no_mangle]
pub unsafe extern "C" fn enif_term_to_binary(env: NIF_ENV, term: NIF_TERM, bin: *mut ErlNifBinary) -> c_int {
    unimplemented!("enif_term_to_binary")
}

#[no_mangle]
pub unsafe extern "C" fn enif_binary_to_term(env: NIF_ENV, data: *const c_uchar, sz: usize, term: *mut NIF_TERM, opts: ErlNifBinaryToTerm) -> usize {
    unimplemented!("enif_binary_to_term")
}

#[no_mangle]
pub unsafe extern "C" fn enif_port_command(env: NIF_ENV, to_port: *const ErlNifPort, msg_env: NIF_ENV, msg: NIF_TERM) -> c_int {
    unimplemented!("enif_port_command")
}

#[no_mangle]
pub unsafe extern "C" fn enif_thread_type() -> c_int {
    unimplemented!("enif_thread_type")
}

#[no_mangle]
pub unsafe extern "C" fn enif_select(env: NIF_ENV, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *const c_void, pid: *const ErlNifPid, eref: NIF_TERM) -> c_int {
    unimplemented!("enif_select")
}

#[no_mangle]
pub unsafe extern "C" fn enif_open_resource_type_x(env: NIF_ENV, name_str: *const c_uchar, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType {
    unimplemented!("enif_open_resource_type_x")
}

#[no_mangle]
pub unsafe extern "C" fn enif_monitor_process(env: NIF_ENV, obj: *const c_void, pid: *const ErlNifPid, monitor: *mut ErlNifMonitor) -> c_int {
    unimplemented!("enif_monitor_process")
}

#[no_mangle]
pub unsafe extern "C" fn enif_demonitor_process(env: NIF_ENV, obj: *const c_void,  monitor: *const ErlNifMonitor) -> c_int {
    unimplemented!("enif_demonitor_process")
}

#[no_mangle]
pub unsafe extern "C" fn enif_compare_monitors(monitor1: *const ErlNifMonitor, monitor2: *const ErlNifMonitor) -> c_int {
    unimplemented!("enif_compare_monitors")
}

#[no_mangle]
pub unsafe extern "C" fn enif_hash(hashtype: ErlNifHash, term: NIF_TERM, salt: u64) -> u64 {
    unimplemented!("enif_hash")
}

#[no_mangle]
pub unsafe extern "C" fn enif_whereis_pid(env: NIF_ENV, name: NIF_TERM, pid: *mut ErlNifPid) -> c_int {
    unimplemented!("enif_whereis_pid")
}

#[no_mangle]
pub unsafe extern "C" fn enif_whereis_port(env: NIF_ENV, name: NIF_TERM, port: *mut ErlNifPort) -> c_int {
    unimplemented!("enif_whereis_port")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_map_from_arrays(env: NIF_ENV, keys: *const NIF_TERM, values: *const NIF_TERM, cnt: usize, map_out: *mut NIF_TERM) -> c_int {
    unimplemented!("enif_make_map_from_arrays")
}

#[no_mangle]
pub unsafe extern "C" fn enif_term_type(env: NIF_ENV, term: *const NIF_TERM) -> ErlNifTermType {
    unimplemented!("enif_term_type")
}

#[no_mangle]
pub unsafe extern "C" fn enif_is_pid_undefined(pid: *const ErlNifPid) -> c_int {
    unimplemented!("enif_is_pid_undefined")
}

#[no_mangle]
pub unsafe extern "C" fn enif_set_pid_undefined(pid: *mut ErlNifPid) {
    unimplemented!("enif_set_pid_undefined")
}

#[no_mangle]
pub unsafe extern "C" fn enif_make_monitor_term(env: NIF_ENV, mon: *const ErlNifMonitor) -> NIF_TERM {
    unimplemented!("enif_make_monitor_term")
}
