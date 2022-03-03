use std::{cmp::Reverse, fmt, iter::FromIterator, cell::RefCell, collections::HashMap};

use regex::Regex;
use rustc_hash::FxHashMap;

use crate::{wimpl::{Body, Expr, Module, Stmt, Var}, highlevel::{StoreOp, LoadOp}};

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct VarExprMap(FxHashMap<Var, Option<Expr>>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarExprMapResult<'a> {
    Precise(&'a Expr),
    Top,
    Uninitialized(Var),
}

impl VarExprMap {
    pub fn from_body(body: &Body) -> Self {
        let mut map = Self::default();

        body.visit_stmt_pre_order(|stmt| {
            if let Stmt::Assign { lhs, type_: _, rhs } = stmt {
                map.add(lhs, rhs)
            }
        });

        map
    }

    pub fn add(&mut self, var: &Var, expr: &Expr) {
        self.0
            .entry(*var)
            // Overapproximate if there was already a prior assignment to that variable.
            .and_modify(|old_expr| *old_expr = None)
            .or_insert_with(|| Some(expr.clone()));
    }

    pub fn get(&self, var: &Var) -> VarExprMapResult {
        match self.0.get(var) {
            // Expression itself refers to a variable, resolve that recursively:
            Some(Some(Expr::VarRef(var))) => self.get(var),
            // Non-recursive case: non-var expression.
            Some(Some(other_expr)) => VarExprMapResult::Precise(other_expr),
            // Overapproximated (e.g., because the variable was assigned twice):
            Some(None) => VarExprMapResult::Top,
            // Uninitialized variable, e.g., parameter:
            None => VarExprMapResult::Uninitialized(*var),
        }
    }
}

impl fmt::Display for VarExprMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (var, maybe_expr) in &self.0 {
            match maybe_expr {
                Some(expr) => writeln!(f, "    {var}: {expr},")?,
                None => writeln!(f, "    {var}: Top,")?,
            }
        }
        write!(f, "}}")
    }
}

/// Returns a slightly abstracted form of the call_indirect expressions, sorted descending by count.
pub fn collect_call_indirect_idx_expr(module: &Module) -> Vec<(String, usize)> {
    let mut result: FxHashMap<String, usize> = FxHashMap::default();
    for func in &module.functions {
        use Expr::*;
        func.body.visit_expr_pre_order(|expr| {
            if let CallIndirect { type_: _, table_idx, args: _ } = expr {
                let table_idx = abstract_expr(table_idx);
                *result.entry(table_idx).or_default() += 1;
            }
        });
    }
    sort_map_count(&result)
}

/// Returns a slightly abstracted form of the call_indirect expressions, sorted descending by count.
pub fn collect_i32_load_store_addr_expr(module: &Module) -> (
    /* addrs */ Vec<(String, usize)>, 
    /* store values */ Vec<(String, usize)>,
 ) {
    let addrs: RefCell<FxHashMap<String, usize>> = RefCell::new(FxHashMap::default());
    let mut values: FxHashMap<String, usize> = FxHashMap::default();
    for func in &module.functions {
        use crate::wimpl::Expr::*;
        use crate::wimpl::Stmt::*;
        // TODO / FIXME Can we make the assumption that call_indirect idx are always loaded/stored
        // via full i32s?
        func.body.visit_pre_order(|expr| {
            if let Store { op: StoreOp::I32Store, memarg: _, addr, value } = expr {
                *addrs.borrow_mut().entry(abstract_expr(addr)).or_default() += 1;
                *values.entry(abstract_expr(value)).or_default() += 1;
            }
        },
        |expr| {
            if let Load { op: LoadOp::I32Load, memarg: _, addr } = expr {
                *addrs.borrow_mut().entry(abstract_expr(addr)).or_default() += 1;
            }
        });
    }
    (sort_map_count(&addrs.into_inner()), sort_map_count(&values))
}

pub fn sort_map_count<T: Ord + Clone, Hasher>(map: &HashMap<T, usize, Hasher>) -> Vec<(T, usize)> {
    let mut vec = Vec::from_iter(map.iter().map(|(t, count)| (t.clone(), *count)));
    vec.sort_by_key(|(expr, count)| Reverse((*count, expr.clone())));
    vec
}

// HACK Remove some stuff that is irrelevant for our analysis
pub fn abstract_expr(expr: &Expr) -> String {
    let expr = expr.to_string();

    lazy_static::lazy_static! {
        static ref MEMARG: Regex = Regex::new(r"\s+offset=\d+\s+").unwrap();
        static ref PARAM: Regex = Regex::new(r"p\d+").unwrap();
        static ref STACK: Regex = Regex::new(r"s\d+").unwrap();
        static ref LOCAL: Regex = Regex::new(r"l\d+").unwrap();
        static ref CONST: Regex = Regex::new(r"const \d+").unwrap();
    }
    let expr = MEMARG.replace_all(&expr, "");
    let expr = PARAM.replace_all(&expr, "<param>");
    let expr = STACK.replace_all(&expr, "<stack>");
    let expr = LOCAL.replace_all(&expr, "<local>");
    let expr = CONST.replace_all(&expr, "const <const>");

    expr.to_string()
}
