use std::{fmt, hash};
use std::convert::TryInto;
use std::marker::PhantomData;

use binary_derive::WasmBinary;
use ordered_float::OrderedFloat;
use serde::{Serialize, Serializer};

use crate::WasmBinary;

/* AST nodes common to high- and low-level representations. */

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// WebAssembly primitive values.
pub enum Val {
    I32(i32),
    I64(i64),
    // Wrap floats, such that they can be ordered and compared (unlike IEEE754 floats),
    // to make it possible, e.g., to put instructions in HashSets etc.
    // TODO replace those with bitpatterns of the floats, similar to wasmparser::Ieee32 and 64
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
}

impl Val {
    pub fn to_type(&self) -> ValType {
        match *self {
            Val::I32(_) => ValType::I32,
            Val::I64(_) => ValType::I64,
            Val::F32(_) => ValType::F32,
            Val::F64(_) => ValType::F64,
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::I32(v) => write!(f, "{}", v),
            Val::I64(v) => write!(f, "{}", v),
            Val::F32(v) => write!(f, "{}", v.into_inner()),
            Val::F64(v) => write!(f, "{}", v.into_inner()),
        }
    }
}


/* Types */

#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ValType {
    #[tag = 0x7f] I32,
    #[tag = 0x7e] I64,
    #[tag = 0x7d] F32,
    #[tag = 0x7c] F64,
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?}", self).to_lowercase())
    }
}

impl ValType {
    pub fn to_char(self) -> char {
        match self {
            ValType::I32 => 'i',
            ValType::I64 => 'I',
            ValType::F32 => 'f',
            ValType::F64 => 'F',
        }
    }
}

#[derive(WasmBinary, Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
#[tag = 0x60]
// TODO implement interning, i.e., use basically Arc<(Vec, Vec)> to share all equal function types.
// or use crate https://crates.io/crates/internment
// TODO would then also need to add params() and results() accessors
// downside: no longer mutable, but right now isn't anyway, and also just not frequently that you
// modify an existing function type.
pub struct FunctionType {
    // Use Box instead of Vec to save the capacity field (smaller size of the struct), since
    // funtion types are immutable anyway (i.e., no dynamic adding/removing of input/result types).
    pub params: Box<[ValType]>,
    pub results: Box<[ValType]>,
}

impl FunctionType {
    pub fn new(params: &[ValType], results: &[ValType]) -> Self {
        FunctionType { params: params.into(), results: results.into() }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?} -> {:?}", self.params, self.results).to_lowercase())
    }
}

// TODO replace all occurrences with FunctionType once we support non-MVP binaries, then remove.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BlockType(pub Option<ValType>);

impl fmt::Display for BlockType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(ty) => write!(f, "[{}]", ty),
            None => write!(f, "[]"),
        }
    }
}

#[derive(WasmBinary, Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct TableType(pub ElemType, pub Limits);

// TODO remove once low-level parser is replaced by wasmparser.
// TODO or rename Anyref to Funcref
#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum ElemType {
    // only value in WASM version 1
    #[tag = 0x70]
    Anyfunc,
}

// TODO replace with just limits
#[derive(WasmBinary, Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct MemoryType(pub Limits);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Limits {
    pub initial_size: u32,
    pub max_size: Option<u32>,
}

#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct GlobalType(pub ValType, pub Mutability);

impl fmt::Display for GlobalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            Mutability::Const => write!(f, "{}", self.0),
            Mutability::Mut => write!(f, "mut {}", self.0),
        }
    }
}

#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Mutability {
    #[tag = 0x00] Const,
    #[tag = 0x01] Mut,
}


/* Indices */

/// A WebAssembly index into one of the possible "index spaces" (e.g., functions, globals, etc.)
/// 
/// The type parameter T is only used for static distinction between the different index spaces, 
/// but has no representation at runtime.
/// Since we don't own T, we don't want a "drop check" and must use fn() -> T, as is
/// recommended in the rustonomicon: https://doc.rust-lang.org/beta/nomicon/phantom-data.html
pub struct Idx<T>(u32, PhantomData<fn() -> T>);

impl<T> Idx<T> {
    // TODO replace with two functions, `to_u32` and `to_usize` (the latter is useful, e.g., when
    // indexing into vectors).
    pub fn into_inner(self) -> usize { self.0 as usize }

    #[inline]
    pub fn to_u32(self) -> u32 { self.0 }
}

// TODO replace with TryFrom with custom NonU32IndexError
// Why accept + convert to a usize if its anyway always u32?
impl<T> From<usize> for Idx<T> {
    #[inline]
    fn from(u: usize) -> Self {
        Idx(u.try_into().expect("wasm32 only allows u32 indices"), PhantomData)
    }
}

impl<T> From<u32> for Idx<T> {
    #[inline]
    fn from(u: u32) -> Self {
        Idx(u, PhantomData)
    }
}

// Custom `Debug`: print a human-readable version of the index space T, but don't print PhantomData.
// E.g. print `Idx<Function>(3, PhantomData)` as `Function 3`
impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_name = std::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "{} {}", type_name, self.0)
    }
}

// Implement some traits manually, because derive adds unnecessary requirements due to the `T` type
// parameter, which we don't want. (T is only a marker and not actually contained in Idx<T>.)
impl<T> Clone for Idx<T> {
    #[inline]
    fn clone(&self) -> Self { self.into_inner().into() }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Idx<T>) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Idx<T> {}

impl<T> hash::Hash for Idx<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Idx<T>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Serialize for Idx<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

// Similar to indices, labels are just a typed wrapper around numbers in the binary format.
// TODO make consistent with Idx: make field private, use into_inner().
#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Label(pub u32);

impl Label {
    pub fn to_u32(self) -> u32 {
        self.0
    }
}

impl Serialize for Label {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

/* Code */

#[derive(WasmBinary, Debug, Copy, Clone, Default, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Memarg {
    /// The alignment of load/stores is just a hint for the VM that says "the effective address of
    /// this load/store should be aligned to <alignment>".
    /// However, if that hint is wrong and the actual address is not aligned, the load/store still
    /// produces the same behavior, potentially just much slower.
    /// (Since the underlying architecture might issue a trap/signal/exception that must be handled.)
    ///
    /// In the binary, the alignment is stored as the exponent of a power of 2.
    /// That is, the actual alignment value will be 2^alignment_exp.
    ///
    /// The actual alignment must be a power of 2 and smaller or equal to the "natural alignment"
    /// of the load/store instruction.
    /// The default alignment (e.g., if none is given in the text format) is the natural alignment
    /// (not zero!).
    ///
    /// See https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions
    /// and https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions.
    pub alignment_exp: u8,
    pub offset: u32,
}

impl Memarg {
    pub fn alignment(self) -> u32 {
        2u32.pow(self.alignment_exp as u32)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RawCustomSection {
    pub name: String,
    pub content: Vec<u8>,
    /// The last non-custom section _before_ this custom section. 
    /// Used during serialization to place the custom section at the right order/position.
    /// If there are multiple custom sections after each other, this will be `None`, 
    /// but the custom sections' relative order will be correct, and the first one
    /// will have this set.
    pub after: Option<SectionId>,
}
// Order is important! Follows the ordering of sections in the binary format
// (except for custom sections, which can appear anywhere).
// https://webassembly.github.io/spec/core/binary/modules.html#binary-module
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum SectionId {
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
    Custom(String),
}

// TODO Remove once the old low-level parser code is gone.
impl SectionId {
    pub fn from_section(section: &crate::lowlevel::Section) -> Self {
        use crate::lowlevel::Section::*;
        use crate::lowlevel::CustomSection;
        match section {
            Type(_) => SectionId::Type,
            Import(_) => SectionId::Import,
            Function(_) => SectionId::Function,
            Table(_) => SectionId::Table,
            Memory(_) => SectionId::Memory,
            Global(_) => SectionId::Global,
            Export(_) => SectionId::Export,
            Start(_) => SectionId::Start,
            Element(_) => SectionId::Element,
            Code(_) => SectionId::Code,
            Data(_) => SectionId::Data,
            Custom(CustomSection::Name(_)) => SectionId::Custom("name".to_string()),
            Custom(CustomSection::Raw(RawCustomSection { name, .. })) => SectionId::Custom(name.clone()),
        }
    }
}