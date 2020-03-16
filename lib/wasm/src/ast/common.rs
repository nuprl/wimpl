use std::cmp::Ordering;
use std::convert::TryInto;
use std::fmt::{self, Write};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use binary_derive::WasmBinary;
use serde::{Serialize, Serializer};

use crate::binary::WasmBinary;

/* AST nodes common to high- and low-level representations. */

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
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
            Val::F32(v) => write!(f, "{}", v),
            Val::F64(v) => write!(f, "{}", v),
        }
    }
}


/* Types */

#[derive(WasmBinary, Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize)]
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
    pub fn to_char(&self) -> char {
        match self {
            ValType::I32 => 'i',
            ValType::I64 => 'I',
            ValType::F32 => 'f',
            ValType::F64 => 'F',
        }
    }
}

#[derive(WasmBinary, Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize)]
#[tag = 0x60]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct BlockType(pub Option<ValType>);

#[derive(WasmBinary, Debug, Clone)]
pub struct TableType(pub ElemType, pub Limits);

#[derive(WasmBinary, Debug, Copy, Clone)]
pub enum ElemType {
    // only value in WASM version 1
    #[tag = 0x70]
    Anyfunc,
}

#[derive(WasmBinary, Debug, Clone)]
pub struct MemoryType(pub Limits);

#[derive(Debug, Copy, Clone)]
pub struct Limits {
    pub initial_size: u32,
    pub max_size: Option<u32>,
}

#[derive(WasmBinary, Debug, Copy, Clone)]
pub struct GlobalType(pub ValType, pub Mutability);

#[derive(WasmBinary, Debug, Copy, Clone)]
pub enum Mutability {
    #[tag = 0x00] Const,
    #[tag = 0x01] Mut,
}


/* Indices */

#[derive(WasmBinary)]
// T is only used for static distinction between different index spaces, but has no representation
// at runtime. Since we don't own T, we don't want a "drop check" and must use fn() -> T, as is
// recommended in the rustonomicon: https://doc.rust-lang.org/beta/nomicon/phantom-data.html
pub struct Idx<T>(u32, PhantomData<fn() -> T>);

impl<T> Idx<T> {
    pub fn into_inner(self) -> usize { self.0 as usize }
}

impl<T> From<usize> for Idx<T> {
    #[inline]
    fn from(u: usize) -> Self {
        Idx(u.try_into().expect("wasm32 only allows u32 indices"), PhantomData)
    }
}

// custom Debug: print index type T, don't print PhantomData
// e.g. Idx<Function>(3, PhantomData) as "Function 3"
impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let type_name = std::any::type_name::<T>().split("::").last().unwrap();
        f.write_str(type_name)?;
        f.write_char(' ')?;
        self.0.fmt(f)
    }
}

// implement some traits manually, since derive(Copy/Eq) add requirements like T: Clone/PartialEq,
// which we do not want (T is only a marker and not actually contained).
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

impl<T> Hash for Idx<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> Serialize for Idx<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Idx<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

// Similar to indices, labels are just a typed wrapper around numbers in the binary format.
#[derive(WasmBinary, Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Label(pub u32);

impl Serialize for Label {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

/* Code */

#[derive(WasmBinary, Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
pub struct Memarg {
    pub alignment: u32,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub struct RawCustomSection {
    pub name: String,
    pub content: Vec<u8>,
    /// Used again during serialization to place the custom section at the right order/position.
    /// The last non-custom section _before_ this custom section. If there are multiple custom
    /// sections after each other, this will not include it, but their relative order will
    /// be respected in the high-level custom section list.
    pub after: Option<std::mem::Discriminant<super::lowlevel::Section>>,
}