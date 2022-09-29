//! Because function types are commonly created, compared, copied etc. we have a very optimized
//! representation for them.
//! The goals are:
//! - Use at most 32 bits of memory, because they are frequently part of larger
//! AST data types, e.g., in functions and instructions.
//! - Should be cheap to compare.
//! - Should be cheap to copy, ideally just a memcpy (Rust: Copy trait)
//! - Should be cheap to create, which is espaclly common in parsing and type checking.
//! 
//! The first solution was to use some form of interning (a global arena for all function types).
//! The problem was that creating lots of function types was slow, because it had to create the
//! non-interned version first before comparing.

use std::{num::*, collections::HashMap, sync::{Mutex, RwLock}};

use once_cell::sync::{OnceCell, Lazy};

// use crate::ValType;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum FunctionType {
    GoedelNumber {
        // Split into inputs and results (instead of a single Gödel number for both)
        // because this way, we can reuse the same lookup table for both, which
        // gets massively smaller than a single table with 2^16 entries.
        inputs: u16,
        results: u8
    },
    ArenaAllocated {
        // TODO Once the niche optimizations are improved in later versions of the Rust language
        // (https://github.com/rust-lang/rust/issues/46213), this could be a NonZeroU32, to make
        // it a bit easier/idiomatic to work with.
        // But even with 3 bytes (2^24 = 16.7 million possible ids), it should be more then enough
        // space for a realistic amount of unique function types.
        id: [u8; 3]
    }
}

#[test]
fn function_type_size() {
    assert_eq!(std::mem::size_of::<FunctionType>(), 4);
    assert_eq!(std::mem::size_of::<Option<FunctionType>>(), 4);
    assert_eq!(std::mem::size_of::<Option<Option<FunctionType>>>(), 4);
}

impl FunctionType {
    pub fn new(inputs: &[ValType], results: &[ValType]) -> Self {
        let inputs_goedel_number = val_type_seq_to_goedel_number(inputs);
        if let Ok(inputs) = u16::try_from(inputs_goedel_number) {
            let results_goedel_number = val_type_seq_to_goedel_number(results);
            if let Ok(results) = u8::try_from(results_goedel_number) {
                return FunctionType::GoedelNumber { inputs, results };
            }
        }
        // Fallback: always succeeds, but is slower.
        Self::new_arena(inputs, results)
    }

    fn new_arena(inputs: &[ValType], results: &[ValType]) -> Self {
        let id = ARENA.get_or_insert(inputs, results);
        let id = usize_to_array(id);
        FunctionType::ArenaAllocated { id }
    }

    pub fn inputs(&self) -> &'static [ValType] {
        match self {
            FunctionType::GoedelNumber { inputs, results: _ } => {
                LOOKUP_TABLE[*inputs as usize]
            },
            FunctionType::ArenaAllocated { id } => {
                let id = array_to_usize(*id);
                ARENA.get(id).0
            },
        }
    }

    pub fn results(&self) -> &'static [ValType] {
        match self {
            FunctionType::GoedelNumber { inputs: _, results } => {
                LOOKUP_TABLE[*results as usize]
            },
            FunctionType::ArenaAllocated { id } => {
                let id = array_to_usize(*id);
                ARENA.get(id).1
            },
        }
    }
}

#[test]
fn inspect_function_types() {
    println!("{:?}", FunctionType::new(&[], &[]));
    println!("{:?}", FunctionType::new(&[ValType::I32], &[]));
    println!("{:?}", FunctionType::new(&[], &[ValType::I32]));
    println!("{:?}", FunctionType::new(&[], &[ValType::I32, ValType::I32, ValType::I32, ValType::I32]));
    println!("{:?}", FunctionType::new(&[], &[ValType::I32, ValType::I32, ValType::I32, ValType::I64]));
    println!("{:?}", FunctionType::new(&[], &[ValType::I32, ValType::I32, ValType::I32, ValType::I32]));
}


// Forward direction: ValType slice to Gödel number.

const fn val_type_to_goedel_number(val_type: ValType) -> usize {
    match val_type {
        ValType::I32 => 0,
        ValType::I64 => 1,
        ValType::F32 => 2,
        ValType::F64 => 3,
        ValType::V128 => 4,
        ValType::Ref => 5,
    }
}

const fn goedel_number_to_val_type(goedel_number: usize) -> Option<ValType> {
    match goedel_number {
        0 => Some(ValType::I32),
        1 => Some(ValType::I64),
        2 => Some(ValType::F32),
        3 => Some(ValType::F64),
        4 => Some(ValType::V128),
        5 => Some(ValType::Ref),
        _ => None
    }
}

// Determined by the number of variants of `ValType`.
const VAL_TYPE_MAX_GOEDEL_NUMBER: usize = 5;

// This is a geometric series, e.g., for 6 possible values it is:
// 1 (for the empty sequence) 
// + 6 (for the sequence with one element) 
// + 36 ...
// = (1 - 6^(max_seq_len+1)) / (1 - 6)
const fn val_type_seq_max_goedel_number(max_seq_len: u32) -> usize {
    let goedel_number_count = ((VAL_TYPE_MAX_GOEDEL_NUMBER + 1).pow(max_seq_len + 1) - 1) / VAL_TYPE_MAX_GOEDEL_NUMBER;
    goedel_number_count - 1
}

#[test]
fn test_goedel_number_constants() {
    assert_eq!(val_type_to_goedel_number(ValType::I32), 0);
    assert_eq!(val_type_to_goedel_number(ValType::F64), 3);
    assert_eq!(val_type_seq_max_goedel_number(0), 0);
    assert_eq!(val_type_seq_max_goedel_number(1), 6);
    assert_eq!(val_type_seq_max_goedel_number(2), 42);
    assert_eq!(val_type_seq_max_goedel_number(3), 258);
}

const fn val_type_seq_to_goedel_number(seq: &[ValType]) -> usize {
    let mut result = 0usize;

    // Cannot (yet) use `for` loop in const fn.
    let mut i = 0;
    while i < seq.len() {
        let val_type = seq[i];
        result *= VAL_TYPE_MAX_GOEDEL_NUMBER + 1;
        result += val_type_to_goedel_number(val_type) + 1;
        i += 1;
    }

    result
}

#[test]
fn test_val_type_seq_to_goedel_number() {
    assert_eq!(val_type_seq_to_goedel_number(&[]), 0);
    assert_eq!(val_type_seq_to_goedel_number(&[ValType::I32]), 1);
    assert_eq!(val_type_seq_to_goedel_number(&[ValType::Ref]), 6);
    assert_eq!(val_type_seq_to_goedel_number(&[ValType::I32, ValType::I32]), 7);
}


// Reverse direction: Gödel number to slice.
// Use lookup table with statically allocated slices, because then there is less computation
// and in particular no memory allocation for those slices.
// Use the same lookup table for inputs and results.

const LOOKUP_TABLE_SIZE: usize = u16::MAX as usize + 1;

type LookupType = [&'static [ValType]; LOOKUP_TABLE_SIZE];
static LOOKUP_TABLE: Lazy<LookupType> = Lazy::new(|| {
    let mut table = [[].as_slice(); LOOKUP_TABLE_SIZE];

    for i in 0..LOOKUP_TABLE_SIZE {
        let seq = goedel_number_to_val_type_seq(i);
        table[i] = seq.leak();
    }

    table
});

#[test]
fn lookup_table_size() {
    println!("number of entries: {}", LOOKUP_TABLE_SIZE);
    println!("size of table: {}", std::mem::size_of_val(LOOKUP_TABLE.as_ref()));
    println!("sum of slice lens: {}", LOOKUP_TABLE.as_ref().iter().map(|seq| seq.len()).sum::<usize>());
}

#[test]
#[ignore]
fn inspect_lookup_table() {
    println!("{:?}", *LOOKUP_TABLE);
}

fn goedel_number_to_val_type_seq(mut goedel_number: usize) -> Vec<ValType> {
    let mut result = Vec::new();

    const DIVISOR: usize = VAL_TYPE_MAX_GOEDEL_NUMBER + 1;
    while goedel_number > 0 {
        goedel_number -= 1;
        let remainder = goedel_number % DIVISOR;
        goedel_number /= DIVISOR;
        let val_type = goedel_number_to_val_type(remainder).unwrap();
        result.insert(0, val_type);
    }

    result.shrink_to_fit();
    result
}

#[test]
fn test_goedel_number_to_val_type_seq() {
    assert_eq!(goedel_number_to_val_type_seq(0), vec![]);
    assert_eq!(goedel_number_to_val_type_seq(1), vec![ValType::I32]);
    assert_eq!(goedel_number_to_val_type_seq(6), vec![ValType::Ref]);
    assert_eq!(goedel_number_to_val_type_seq(7), vec![ValType::I32, ValType::I32]);
}

#[test]
fn test_goedel_number_roundtrips() {
    for goedel_number in 0..u16::MAX as usize {
        let val_type_seq = goedel_number_to_val_type_seq(goedel_number);
        let roundtrip = val_type_seq_to_goedel_number(&val_type_seq);
        assert_eq!(goedel_number, roundtrip, "{} -> {:?} -> {}", goedel_number, val_type_seq, roundtrip);
    }
}

#[derive(Default)]
struct ArenaInner {
    idx_to_func_type: Vec<(&'static [ValType], &'static [ValType])>,
    func_type_to_idx: HashMap<(&'static [ValType], &'static [ValType]), usize>,
}

#[derive(Default)]
struct Arena(RwLock<ArenaInner>);

static ARENA: Lazy<Arena> = Lazy::new(Arena::default);

impl Arena {
    fn get(&self, id: usize) -> (&'static [ValType], &'static [ValType]) {
        let arena = self.0.read().unwrap();
        let (params, results) = &arena.idx_to_func_type[id];
        (params, results)
    }

    fn get_or_insert(&self, params: &[ValType], results: &[ValType]) -> usize {
        let mut arena = self.0.write().unwrap();
        if let Some(&id) = arena.func_type_to_idx.get(&(params, results)) {
            id
        } else {
            let params = params.to_vec().leak();
            let results = results.to_vec().leak();
            let id = arena.idx_to_func_type.len();
            arena.idx_to_func_type.push((params, results));
            arena.func_type_to_idx.insert((params, results), id);
            id
        }
    }
}

fn array_to_usize(array: [u8; 3]) -> usize {
    (array[0] as usize) << 16 | (array[1] as usize) << 8 | (array[2] as usize)
}

fn usize_to_array(mut value: usize) -> [u8; 3] {
    let mut array = [0u8; 3];
    array[0] = (value >> 16) as u8;
    value &= 0xFFFF;
    array[1] = (value >> 8) as u8;
    value &= 0xFFFF;
    array[2] = value as u8;
    array
}

#[test]
fn test_id_array_roundtrips() {
    for id in 0..2usize.pow(24) {
        let array = usize_to_array(id);
        let roundtrip = array_to_usize(array);
        assert_eq!(id, roundtrip, "{} -> {:?} -> {}", id, array, roundtrip);
    }
}