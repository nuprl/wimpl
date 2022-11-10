//! Conversion from standard WebAssembly to Wimpl.

use std::{
    any::Any, borrow::BorrowMut, collections::HashSet, convert::TryInto, fs::remove_file,
    iter::Map, marker::PhantomData,
};

use wasabi_wasm as wasm;
use wasm::Local;

use crate::*;

/// The mutable state during conversion.
pub struct State<'module> {
    instrs_iter: std::iter::Enumerate<std::slice::Iter<'module, wasm::Instr>>,
    type_checker: TypeChecker<'module>,

    label_count: u32,
    stack_var_count: u32,

    #[allow(clippy::type_complexity)]
    label_stack: Vec<(Label, /* is_loop */ bool, Option<Var>)>,

    instr_loc_map: HashMap<InstrId, WasmSrcLocation>,
    id_stmt_map: HashMap<InstrId, Stmt>,
    id_expr_map: HashMap<InstrId, Expr>,
}

/// The immutable context information required but never mutated during conversion.
#[derive(Clone, Copy)]
pub struct Context<'module> {
    module: &'module wasm::Module,
    func_ty: &'module FunctionType,
    func_idx: Idx<wasm::Function>,
    func_idx_to_id_map: &'module [FunctionId],
}

fn wimplify_instrs<'module>(
    stmts_result: &mut Vec<Stmt>,
    state: &mut State,
    context: Context<'module>,
) -> Result</* was_else */ bool, String> {
    use ExprKind::*;
    use Var::*;

    // State that is "local" to this nested block (unlike `state`, which is for the whole function).
    let mut expr_stack: Vec<(Expr, ValType)> = Vec::new();

    while let Some((instr_idx, instr)) = state.instrs_iter.next() {
        let ty = state
            .type_checker
            .check_next_instr(instr)
            .map_err(|e| e.to_string())?;

        // DEBUG
        // println!("expr stack: [{}]\ninstr: {}, {}", expr_stack.iter().map(|(expr, ty)| format!("{}: {}", expr, ty)).collect::<Vec<_>>().join(", "), instr, ty);

        let ty = match ty {
            // If the following code (until the next end or else) is unreachable,
            // don't generate any Wimpl code from it, so continue until then.
            InferredInstructionType::Unreachable => {
                match instr {
                    // But end and else terminate the current nested block, so return in that case.
                    wasm::Instr::End => {
                        state.label_stack.pop();
                        return Ok(false);
                    }
                    wasm::Instr::Else => {
                        // Unlike for end, don't pop here because the nested "else" block still follows.
                        return Ok(true);
                    }
                    _ => continue,
                }
            }
            InferredInstructionType::Reachable(ty) => ty,
        };

        // Utility functions for the conversion:

        // Insert the current wasm instruction's location as metadata for the given generated
        // wimpl Expr/Stmt.
        let attach_current_wasm_src_location = |state: &mut State, id: InstrId| {
            let wasm_src_location = WasmSrcLocation(context.func_idx, instr_idx.into());
            state.instr_loc_map.insert(id, wasm_src_location);
        };

        // Append a statement to the result and attach the current WebAssembly instruction location
        // as its source location.
        let push_stmt = |stmts_result: &mut Vec<Stmt>, state: &mut State, stmt: StmtKind| {
            let stmt = Stmt::new(stmt);
            attach_current_wasm_src_location(state, stmt.id);
            state.id_stmt_map.insert(stmt.id, stmt.clone());
            stmts_result.push(stmt);
        };

        // Push an expression on the stack and attach the current WebAssembly instruction location
        // as its source location.
        let push_expr = |expr_stack: &mut Vec<(Expr, ValType)>,
                         state: &mut State,
                         expr: ExprKind,
                         ty: ValType| {
            let expr = Expr::new(expr);
            attach_current_wasm_src_location(state, expr.id);
            state.id_expr_map.insert(expr.id, expr.clone());
            expr_stack.push((expr, ty));
        };

        // Only call this function when you have finished translating the instructions, i.e., after
        // you have popped all inputs from the `var_stack`, since this changes `var_stack`.
        fn create_fresh_stack_var(state: &mut State) -> Var {
            let var = Stack(state.stack_var_count);
            state.stack_var_count += 1;
            var
        }

        // To make sure the Wimpl evaluation order is equivalent to WebAssembly, before pushing a
        // statement, we must make sure (by calling this function) that all expressions still
        // "dormant" on the stack are actually executed beforehand (e.g., due to side effects).
        // To be able to refer to their values later on, we replace those expressions by references
        // to the freshly created variables holding the evaluation result instead.
        // This is also required whenever an instruction would "duplicate" an expression on the
        // stack. This is not desired, because it could change semantics (due to side effects) and
        // performance (due to double evaluation). So instead the expression in evaluated once here
        // and the the duplication is just refering to the inserted stack slot with a VarRef.
        fn materialize_all_exprs_as_stmts(
            state: &mut State,
            expr_stack: &mut Vec<(Expr, ValType)>,
            stmts_result: &mut Vec<Stmt>,
        ) {
            for (expr, type_) in expr_stack {
                match expr.kind {
                    VarRef(Stack(_)) => {
                        // Optimization: Expression already is in a stack variable, so no need to
                        // materialize it again (and it also cannot be overwritten since stack
                        // variables are effectively in SSA form, unlike parameters, locals, or
                        // result variables).
                    }
                    Const(_) => {
                        // Optimization: The stack holds only a constant, no need to materialize
                        // it into a variable, since it is side-effect free anyway and it doesn't
                        // save Wimpl code size by not duplicating it (actually the opposite, since
                        // creation and assignment of the materialized stack variable is additional
                        // code).
                    }
                    _ => {
                        // All other cases:
                        // 1. Create a fresh variable
                        // 2. Replace the expression in the stack with a reference to that variable.
                        // 3. Push out a statement, assigning the expression to the fresh variable.

                        let var = create_fresh_stack_var(state);

                        // Source location tracking:
                        // The expression on the stack refers back to the original WebAssembly
                        // instruction that has produced the stack value.
                        // We reuse this source location also for the generated variable reference
                        // and the assignment statement.
                        // In particular, it would be NOT correct to use the source location of the
                        // current instruction that has only prompted us to materialize the
                        // expression.
                        let wasm_src_location = state
                            .instr_loc_map
                            .get(&expr.id)
                            .expect("missing metadata for expression from expression stack")
                            .clone();

                        let varref = Expr::new(VarRef(var));
                        state
                            .instr_loc_map
                            .insert(varref.id, wasm_src_location.clone());
                        state.id_expr_map.insert(varref.id, varref.clone());

                        let expr = std::mem::replace(expr, varref);
                        // The `expr` already has the correct metadata attached, so nothing to do here...

                        let stmt = Stmt::new(StmtKind::Assign {
                            lhs: var,
                            type_: *type_,
                            rhs: expr,
                        });
                        state.instr_loc_map.insert(stmt.id, wasm_src_location);
                        state.id_stmt_map.insert(stmt.id, stmt.clone());

                        stmts_result.push(stmt);
                    }
                }
            }
        }

        fn local_idx_to_var(context: Context, local_idx: Idx<::wasabi_wasm::Local>) -> Var {
            let local_idx = local_idx.to_u32();
            let num_params: u32 = context
                .func_ty
                .inputs()
                .len()
                .try_into()
                .expect("more than 2^32 parameters");
            if local_idx < num_params {
                Param(local_idx)
            } else {
                Local(local_idx - num_params)
            }
        }

        let create_block_label_and_var = |state: &mut State,
                                          expr_stack: &mut Vec<(Expr, ValType)>,
                                          blocktype: BlockType,
                                          is_loop: bool|
         -> Label {
            // Allocate a new label for this block.
            let label = Label(state.label_count);
            state.label_count += 1;

            let result_var = match blocktype.0 {
                Some(type_) => {
                    // Also allocate a new block result variable, with the number matching the
                    // block label number.
                    let result_var = BlockResult(label.0);
                    // The result of the block is then at the top of the stack after the block.
                    push_expr(expr_stack, state, VarRef(result_var), type_);
                    Some(result_var)
                }
                None => None,
            };

            state.label_stack.push((label, is_loop, result_var));

            label
        };

        fn wasm_label_to_wimpl_label_and_block_var(
            state: &State,
            wasm_label: wasm::Label,
        ) -> (Label, Option<Var>) {
            let (wimpl_label, is_loop, block_result) = *state
                .label_stack
                .iter()
                .rev()
                .nth(wasm_label.to_usize())
                .expect("invalid branch label, not in label stack");

            match (is_loop, block_result) {
                // Target block is not a loop and needs a result, so return the result variable.
                (false, Some(block_result_var)) => (wimpl_label, Some(block_result_var)),

                // Target block either has no result, or is a loop. Because loops/blocks have no
                // inputs in Wasm MVP and br to loops jump to the beginning, there is no result
                // that would need to be assigned in that case.
                (true, Some(_)) | (_, None) => (wimpl_label, None),
            }
        }

        // For loads and stores, convert the offset (that is part of the Wasm instruction, but Wimpl
        // doesn't have) to a constant addition on the address.
        // Also drops the memarg alignment hint, since that is only for optimization by the Wasm
        // runtime, but not useful for Wimpl static analysis.
        // (FIXME But that also means Wasm -> Wimpl -> Wasm doesn't roundtrip since information is lost.)
        // Associate the same source location (the Wasm store instruction) with all that we
        // generated, i.e., the constant and the addition.
        let convert_memarg_offset_to_addr_expr =
            |state: &mut State, memarg: Memarg, addr_expr: &mut Expr| {
                if memarg.offset != 0 {
                    let offset = Expr::new(Const(Val::I32(
                        memarg.offset.try_into().expect("u32 to i32"),
                    )));
                    attach_current_wasm_src_location(state, offset.id);
                    state.id_expr_map.insert(offset.id, offset.clone());

                    let add = Expr::new(Binary(
                        BinaryOp::I32Add,
                        Box::new(addr_expr.clone()),
                        Box::new(offset),
                    ));
                    attach_current_wasm_src_location(state, add.id);
                    state.id_expr_map.insert(add.id, add.clone());

                    *addr_expr = add;
                }
            };

        // Conversion of each WebAssembly instruction to (one or more) Wimpl statements:
        match instr {
            wasm::Instr::Unreachable => {
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                push_stmt(stmts_result, state, StmtKind::Unreachable)
            }

            wasm::Instr::Nop => {}

            wasm::Instr::Block(blocktype) => {
                // Do this before any statements are pushed out, or expressions added to the stack
                // (e.g., the block variable holding the result.)
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                let label = create_block_label_and_var(state, &mut expr_stack, *blocktype, false);

                // DEBUG
                // println!("block: {}, {:?}", blocktype, state.label_stack);

                let mut block_body = Vec::new();
                let ends_with_else = wimplify_instrs(&mut block_body, state, context)?;
                assert!(
                    !ends_with_else,
                    "block should be terminated by end, not else"
                );

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Block {
                        body: Body(block_body),
                        end_label: label,
                    },
                );
            }
            wasm::Instr::Loop(blocktype) => {
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                let label = create_block_label_and_var(state, &mut expr_stack, *blocktype, true);

                let mut loop_body = Vec::new();
                let ends_with_else = wimplify_instrs(&mut loop_body, state, context)?;
                assert!(
                    !ends_with_else,
                    "loop should be terminated by end, not else"
                );

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Loop {
                        begin_label: label,
                        body: Body(loop_body),
                    },
                );
            }

            // Translate if into block + if, because the Wimpl if alone cannot have a label/result.
            wasm::Instr::If(blocktype) => {
                let (condition, condition_ty) = expr_stack
                    .pop()
                    .expect("if expects a condition which was not found on the stack");
                assert_eq!(condition_ty, ValType::I32);

                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                let label = create_block_label_and_var(state, &mut expr_stack, *blocktype, false);

                let mut if_body = Vec::new();
                let has_else = wimplify_instrs(&mut if_body, state, context)?;
                let else_body = if has_else {
                    let mut else_body = Vec::new();
                    let ends_with_else = wimplify_instrs(&mut else_body, state, context)?;
                    assert!(
                        !ends_with_else,
                        "else block should be terminated by end not else"
                    );
                    Some(Body(else_body))
                } else {
                    None
                };

                // Wrap `if` inside a `block`, because Wimpl ifs don't have a label, but if a
                // branch wants to exit the if, it needs to target a label.
                let if_stmt = Stmt::new(StmtKind::If {
                    condition,
                    if_body: Body(if_body),
                    else_body,
                });
                attach_current_wasm_src_location(state, if_stmt.id);
                state.id_stmt_map.insert(if_stmt.id, if_stmt.clone());
                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Block {
                        body: Body(vec![if_stmt]),
                        end_label: label,
                    },
                );
            }

            wasm::Instr::Else => {
                // Cannot pop because you still want it to be on the label stack while processing the else body.
                let (_label, is_loop, if_result_var) = *state
                    .label_stack
                    .last()
                    .expect("label stack should include if label");
                assert!(!is_loop, "if block result should never have loop flag set");

                // Assign result of the if statement that we just finished processing.
                if let Some(if_result_var) = if_result_var {
                    let (value, type_) = expr_stack
                        .pop()
                        .expect("else expects if result value on the stack");

                    // Because the stack should be empty at the end of a block, we don't need to
                    // materialize expressions here. See assert below.

                    push_stmt(
                        stmts_result,
                        state,
                        StmtKind::Assign {
                            lhs: if_result_var,
                            type_,
                            rhs: value,
                        },
                    )
                }

                assert!(
                    expr_stack.is_empty(),
                    "should not contain superfluous expressions"
                );

                // End recursive invocation and return converted body of the current block.
                return Ok(true);
            }

            wasm::Instr::End => {
                let (_label, _is_loop, block_result_var) = state
                    .label_stack
                    .pop()
                    .expect("end of a block expects the matching label to be in the label stack");

                if let Some(block_result_var) = block_result_var {
                    let (value, type_) = expr_stack
                        .pop()
                        .expect("end expects if/block/loop result value on the stack");

                    // Because the stack should be empty at the end of a block, we don't need to
                    // materialize expressions here. See assert below.

                    push_stmt(
                        stmts_result,
                        state,
                        StmtKind::Assign {
                            lhs: block_result_var,
                            type_,
                            rhs: value,
                        },
                    );
                };

                assert!(
                    expr_stack.is_empty(),
                    "should not contain superfluous expressions"
                );

                // End recursive invocation and return converted body of the current block.
                return Ok(false);
            }

            wasm::Instr::Return => {
                let (wimpl_label, is_loop, return_var) = *state
                    .label_stack
                    .first()
                    .expect("empty label stack, but expected function ");
                assert_eq!(
                    wimpl_label,
                    Label(0),
                    "first element on the label stack should point to function body label"
                );
                assert!(
                    !is_loop,
                    "function body block should not have loop flag set"
                );

                if let Some(return_var) = return_var {
                    let (return_expr, type_) =
                        expr_stack.pop().expect("return expects a return value");

                    materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                    push_stmt(
                        stmts_result,
                        state,
                        StmtKind::Assign {
                            lhs: return_var,
                            type_,
                            rhs: return_expr,
                        },
                    )
                } else {
                    materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                }

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Br {
                        target: wimpl_label,
                    },
                )
            }

            wasm::Instr::Br(label) => {
                let (wimpl_label, block_var) =
                    wasm_label_to_wimpl_label_and_block_var(state, *label);

                // Handle dataflow ("transported value through branch") by explicit assignment.
                if let Some(block_var) = block_var {
                    let (branch_value_var, type_) =
                        expr_stack.pop().expect("br to this label expects a value");

                    materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                    push_stmt(
                        stmts_result,
                        state,
                        StmtKind::Assign {
                            lhs: block_var,
                            type_,
                            rhs: branch_value_var,
                        },
                    )
                } else {
                    materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                }

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Br {
                        target: wimpl_label,
                    },
                )
            }

            // Translate br_if as if + (unconditional) br.
            wasm::Instr::BrIf(label) => {
                let (condition, condition_ty) = expr_stack
                    .pop()
                    .expect("br_if expects a conditional on the stack");
                assert_eq!(condition_ty, ValType::I32);

                let (wimpl_label, block_var) =
                    wasm_label_to_wimpl_label_and_block_var(state, *label);

                // Materialize also the (maybe) value of the branch target in a variable.
                // This is necessary for br_if unlike for br because the expression is statically
                // duplicated, once for the case if the branch is taken (assigned to target result
                // variable), and once if the branch is not taken (in the expression stack).
                // However, we don't want the expression to be evaluated twice, so assign it to a
                // variable.
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                let mut if_body = Vec::with_capacity(1 + block_var.iter().len());
                if let Some(block_var) = block_var {
                    // The value "transported" by the branch needs to be duplicated, in case the
                    // branch is not taken.
                    // Since we just materialized this, this is going to be a `VarRef` and cloning
                    // it is fine (doesn't alter semantics or introduce duplicate expressions).
                    let (branch_value_var, type_) = expr_stack
                        .last()
                        .expect("br_if to this label expects a value")
                        .clone();

                    push_stmt(
                        &mut if_body,
                        state,
                        StmtKind::Assign {
                            lhs: block_var,
                            type_,
                            rhs: branch_value_var,
                        },
                    );
                }
                push_stmt(
                    &mut if_body,
                    state,
                    StmtKind::Br {
                        target: wimpl_label,
                    },
                );

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::If {
                        condition,
                        if_body: Body(if_body),
                        else_body: None,
                    },
                )
            }

            wasm::Instr::BrTable { table, default } => {
                let (table_index_expr, table_index_ty) = expr_stack
                    .pop()
                    .expect("br_table expects an index into the table on the stack");
                assert_eq!(table_index_ty, ValType::I32);

                // Same as for br_if above.
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                // Similar to br_if above, except that we do this for every target instead of just once.
                let make_case_body = move |label: wasm::Label,
                                           state: &mut State,
                                           expr_stack: &Vec<(Expr, ValType)>|
                      -> Vec<Stmt> {
                    let (wimpl_label, block_var) =
                        wasm_label_to_wimpl_label_and_block_var(state, label);

                    let mut case_body = Vec::with_capacity(1 + block_var.iter().len());
                    if let Some(block_var) = block_var {
                        // Same as for br_if above.
                        let (branch_value_var, type_) = expr_stack
                            .last()
                            .expect("this br_table expects a value")
                            .clone();

                        push_stmt(
                            &mut case_body,
                            state,
                            StmtKind::Assign {
                                lhs: block_var,
                                type_,
                                rhs: branch_value_var,
                            },
                        );
                    }
                    push_stmt(
                        &mut case_body,
                        state,
                        StmtKind::Br {
                            target: wimpl_label,
                        },
                    );

                    case_body
                };

                let switch = StmtKind::Switch {
                    index: table_index_expr,
                    cases: table
                        .iter()
                        .map(|label| Body(make_case_body(*label, state, &mut expr_stack)))
                        .collect(),
                    default: Body(make_case_body(*default, state, &mut expr_stack)),
                };
                push_stmt(stmts_result, state, switch);
            }

            wasm::Instr::Call(func_index) => {
                let n_args = context.module.function(*func_index).type_.inputs().len();
                let call_expr = Call {
                    func: context.func_idx_to_id_map[func_index.to_usize()].clone(),
                    args: expr_stack
                        .split_off(expr_stack.len() - n_args)
                        .into_iter()
                        .map(|(expr, _ty)| expr)
                        .collect(),
                };

                // If the call doesn't produce a value, emit it as a statement for its side-effects,
                // otherwise push its result on the stack.
                match ty.results() {
                    [] => {
                        materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                        // If we create an intermediate statement expression, also attach the source
                        // location information to it (the information for the statement is attached
                        // automatically by `push_stmt()`).
                        let call_expr = Expr::new(call_expr);
                        attach_current_wasm_src_location(state, call_expr.id);
                        state.id_expr_map.insert(call_expr.id, call_expr.clone());
                        push_stmt(stmts_result, state, StmtKind::Expr(call_expr));
                    }
                    [type_] => push_expr(&mut expr_stack, state, call_expr, *type_),
                    _ => panic!("WebAssembly multi-value extension"),
                }
            }

            wasm::Instr::CallIndirect(func_type, table_index) => {
                assert_eq!(
                    table_index.to_usize(),
                    0,
                    "WebAssembly MVP must always have a single table"
                );

                let (table_index_expr, table_index_ty) = expr_stack
                    .pop()
                    .expect("call_indirect expects a table index on the stack");
                assert_eq!(table_index_ty, ValType::I32);

                let n_args = func_type.inputs().len();
                let call_expr = CallIndirect {
                    type_: *func_type,
                    table_idx: Box::new(table_index_expr),
                    args: expr_stack
                        .split_off(expr_stack.len() - n_args)
                        .into_iter()
                        .map(|(expr, _ty)| expr)
                        .collect(),
                };

                // Analoguous to call above.
                match ty.results() {
                    [] => {
                        materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                        let call_expr = Expr::new(call_expr);
                        attach_current_wasm_src_location(state, call_expr.id);
                        state.id_expr_map.insert(call_expr.id, call_expr.clone());
                        push_stmt(stmts_result, state, StmtKind::Expr(call_expr));
                    }
                    [type_] => push_expr(&mut expr_stack, state, call_expr, *type_),
                    _ => panic!("WebAssembly multi-value extension"),
                }
            }

            wasm::Instr::Drop => {
                let expr = expr_stack
                    .pop()
                    .expect("drop expects a value on the stack")
                    .0;

                // Optimization:
                match expr.kind {
                    // Don't generate an expression-statement, if the popped expression is just a
                    // variable reference, i.e., the expression statement won't have any effect.
                    // Also, in that case, we don't execute any code at all, so no need to
                    // materialize the rest of the stack either.
                    VarRef(_) => { /* FIXME Leaks the metadata associated to the dropped `expr`. */
                    }
                    // The same argument also applies to constants.
                    // TODO More generally, one could avoid generating statements for any
                    // expression without side-effects, e.g., via a method Expr::has_side_effect().
                    Const(_) => { /* FIXME Leaks the metadata associated to the dropped `expr`. */ }
                    // Otherwise, emit it as an expression statement, to at least execute it for its
                    // side-effects, e.g., if it was a call.
                    _ => {
                        materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                        push_stmt(stmts_result, state, StmtKind::Expr(expr))
                    }
                }

                // TODO Another alternative is to just materialize everything (including
                // the drop argument, which is NOT materialized above), pop the (now in a variable)
                // argument from the expression stack and then just not use the variable again.
                // This would be slightly more general but creates more stack variables.
                // Finally a dead store/liveness analysis could then remove those variables.
            }

            // Translate as if block assigning to a fresh stack variable.
            wasm::Instr::Select => {
                let (condition, condition_ty) = expr_stack
                    .pop()
                    .expect("select expects a conditional on the stack");
                assert_eq!(condition_ty, ValType::I32);

                let type_ = ty.results()[0];

                // We need to materialize the if and else case, regardless of whether they are used,
                // to match the semantics of Wasm, where both are always evaluated.
                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);
                let else_result_var = expr_stack
                    .pop()
                    .expect("select expects else value on the stack")
                    .0;
                let if_result_var = expr_stack
                    .pop()
                    .expect("select expects if value on the stack")
                    .0;

                // TODO Use a block result variable here for consistency with if block results?
                // But then one have to be VERY careful to increase the label counter as well,
                // because we rely on the invariant that #label == #block result vars.
                let result_var = create_fresh_stack_var(state);
                push_expr(&mut expr_stack, state, VarRef(result_var), type_);

                let if_result_assign = Stmt::new(StmtKind::Assign {
                    lhs: result_var,
                    type_,
                    rhs: if_result_var,
                });
                let else_result_assign = Stmt::new(StmtKind::Assign {
                    lhs: result_var,
                    type_,
                    rhs: else_result_var,
                });
                // Use the same source location for the generated statements.
                attach_current_wasm_src_location(state, if_result_assign.id);
                state
                    .id_stmt_map
                    .insert(if_result_assign.id, if_result_assign.clone());
                attach_current_wasm_src_location(state, else_result_assign.id);
                state
                    .id_stmt_map
                    .insert(else_result_assign.id, else_result_assign.clone());
                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::If {
                        condition,
                        if_body: Body(vec![if_result_assign]),
                        else_body: Some(Body(vec![else_result_assign])),
                    },
                );
            }

            wasm::Instr::Local(wasm::LocalOp::Get, local_idx) => {
                let type_ = ty.results()[0];

                push_expr(
                    &mut expr_stack,
                    state,
                    VarRef(local_idx_to_var(context, *local_idx)),
                    type_,
                );
            }

            wasm::Instr::Local(wasm::LocalOp::Set, local_idx) => {
                let (rhs, type_) = expr_stack
                    .pop()
                    .expect("local.set expects a value on the stack");
                assert_eq!(type_, ty.inputs()[0]);

                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Assign {
                        lhs: local_idx_to_var(context, *local_idx),
                        type_,
                        rhs,
                    },
                )
            }

            // Essentially equivalent to a local.set followed by a local.get (see above).
            wasm::Instr::Local(wasm::LocalOp::Tee, local_idx) => {
                let (value, type_) = expr_stack
                    .pop()
                    .expect("local.tee expects a value on the stack");
                assert_eq!(type_, ty.inputs()[0]);

                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                let local_var = local_idx_to_var(context, *local_idx);
                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Assign {
                        lhs: local_var,
                        type_,
                        rhs: value,
                    },
                );
                push_expr(&mut expr_stack, state, VarRef(local_var), type_);
            }

            wasm::Instr::Global(wasm::GlobalOp::Get, global_idx) => {
                let type_ = ty.results()[0];

                push_expr(
                    &mut expr_stack,
                    state,
                    VarRef(Global(global_idx.to_u32())),
                    type_,
                );
            }

            wasm::Instr::Global(wasm::GlobalOp::Set, global_idx) => {
                let (rhs, type_) = expr_stack
                    .pop()
                    .expect("local.set expects a value on the stack");
                assert_eq!(type_, ty.inputs()[0]);

                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Assign {
                        lhs: Global(global_idx.to_u32()),
                        type_,
                        rhs,
                    },
                );
            }

            wasm::Instr::Load(loadop, memarg) => {
                let (mut addr, addr_ty) = expr_stack
                    .pop()
                    .expect("load expects an address on the stack");
                assert_eq!(addr_ty, ValType::I32);

                convert_memarg_offset_to_addr_expr(state, *memarg, &mut addr);

                let type_ = ty.results()[0];
                push_expr(
                    &mut expr_stack,
                    state,
                    Load {
                        op: *loadop,
                        addr: Box::new(addr),
                    },
                    type_,
                );
            }

            wasm::Instr::Store(op, memarg) => {
                let (value, value_ty) = expr_stack
                    .pop()
                    .expect("store expects a value to store on the stack");
                assert_eq!(value_ty, ty.inputs()[1]);

                let (mut addr, addr_ty) = expr_stack
                    .pop()
                    .expect("store expects an address on the stack");
                assert_eq!(addr_ty, ValType::I32);

                convert_memarg_offset_to_addr_expr(state, *memarg, &mut addr);

                materialize_all_exprs_as_stmts(state, &mut expr_stack, stmts_result);

                push_stmt(
                    stmts_result,
                    state,
                    StmtKind::Store {
                        op: *op,
                        addr,
                        value,
                    },
                );
            }

            wasm::Instr::MemorySize(memory_idx) => {
                assert_eq!(
                    memory_idx.to_usize(),
                    0,
                    "Wasm MVP only has a single memory"
                );

                let result_type = ty.results()[0];
                push_expr(&mut expr_stack, state, MemorySize, result_type);
            }

            wasm::Instr::MemoryGrow(memory_idx) => {
                assert_eq!(
                    memory_idx.to_usize(),
                    0,
                    "Wasm MVP only has a single memory"
                );

                let (pages, pages_ty) = expr_stack
                    .pop()
                    .expect("memory.grow expects a value on the stack");
                assert_eq!(pages_ty, ValType::I32);

                let result_type = ty.results()[0];
                push_expr(
                    &mut expr_stack,
                    state,
                    MemoryGrow {
                        pages: Box::new(pages),
                    },
                    result_type,
                );
            }

            wasm::Instr::Const(val) => {
                push_expr(&mut expr_stack, state, Const(*val), val.to_type());
            }

            wasm::Instr::Unary(op) => {
                let (arg, type_) = expr_stack
                    .pop()
                    .expect("unary operation expects argument on the stack");
                assert_eq!(type_, ty.inputs()[0]);

                let result_type = ty.results()[0];
                push_expr(
                    &mut expr_stack,
                    state,
                    Unary(*op, Box::new(arg)),
                    result_type,
                );
            }

            wasm::Instr::Binary(op) => {
                let (right, type_) = expr_stack
                    .pop()
                    .expect("binary operation expects right argument on the stack");
                assert_eq!(type_, ty.inputs()[1]);

                let (left, type_) = expr_stack
                    .pop()
                    .expect("binary operation expects left argument on the stack");
                assert_eq!(type_, ty.inputs()[0]);

                let result_type = ty.results()[0];
                push_expr(
                    &mut expr_stack,
                    state,
                    Binary(*op, Box::new(left), Box::new(right)),
                    result_type,
                );
            }
        }
    }

    Ok(false)
}

fn wimplify_function_body(
    function: &wasm::Function,
    func_idx: Idx<wasm::Function>,
    module: &wasm::Module,
    module_metadata: &mut Metadata,
    func_orig_idx_to_id_map: &[FunctionId],
) -> Result<Option<Body>, String> {
    if let Some(code) = function.code() {
        // The body will be at least the number of locals and often a nop or return instruction.
        let mut stmts_result: Vec<Stmt> = Vec::with_capacity(function.local_count() + 1);

        let mut id_stmt_map = HashMap::new();

        // Initialize the local variables.
        for (local_idx, loc) in function.locals() {
            let (loc_name, loc_type) = (&loc.name, loc.type_);
            // FIXME Was crashing, which is too harsh just because names are missing.
            // if let Some(_loc_name) = loc_name {
            //     todo!("you haven't yet implemented locals having names");
            // } else {
            let local_var_initalization_stmt = Stmt::new(StmtKind::Assign {
                lhs: Var::Local(local_idx.to_u32() - function.type_.inputs().len() as u32),
                rhs: ExprKind::Const(loc_type.zero()).into(),
                type_: loc_type,
            });
            // Note that we do not attach metadata (Wasm source location info) to these generated
            // assignments, because they are implicit, due to the zero-initialization of locals in
            // WebAssembly.
            // TODO: we want id -> stmt metadata tho
            id_stmt_map.insert(
                local_var_initalization_stmt.id,
                local_var_initalization_stmt.clone(),
            );

            stmts_result.push(local_var_initalization_stmt);
            // }
        }

        // Translate the instructions in the function:
        let context = Context {
            module,
            func_ty: &function.type_,
            func_idx,
            func_idx_to_id_map: func_orig_idx_to_id_map,
        };

        let mut state = State {
            instrs_iter: code.body.as_slice().iter().enumerate(),
            type_checker: TypeChecker::begin_function(function, module),
            label_stack: Vec::new(),
            label_count: 1, // 0 is already used by the function body block.
            stack_var_count: 0,
            instr_loc_map: HashMap::new(),
            id_stmt_map: HashMap::new(),
            id_expr_map: HashMap::new(),
        };

        let return_var = match function.type_.results() {
            [] => None,
            [_type] => Some(Var::Return(0)),
            _ => unimplemented!("only WebAssembly MVP is supported, not multi-value extension"),
        };
        state.label_stack.push((Label(0), false, return_var));

        let was_else = wimplify_instrs(&mut stmts_result, &mut state, context)?;
        assert!(!was_else, "function should not end with else");

        module_metadata
            .instr_location_map
            .extend(state.instr_loc_map.into_iter());
        module_metadata.id_stmt_map.extend(id_stmt_map.into_iter());
        module_metadata
            .id_stmt_map
            .extend(state.id_stmt_map.into_iter());

        Ok(Some(Body(stmts_result)))
    } else {
        Ok(None)
    }
}

pub fn wimplify(module: &wasm::Module) -> Result<Module, String> {
    // Generate unique function ids for each function in the module.
    let func_orig_idx_to_id_map = FunctionId::from_module(module);

    let mut metadata = Metadata {
        instr_location_map: HashMap::new(),
        id_stmt_map: HashMap::new(),
        id_expr_map: HashMap::new(),
        func_id_to_orig_idx_map: HashMap::new(),
    };

    // TODO parallelize
    let functions = module
        .functions()
        .map(|(func_idx, function)| -> Result<Function, String> {
            let name = func_orig_idx_to_id_map[func_idx.to_usize()].clone();
            metadata
                .func_id_to_orig_idx_map
                .insert(name.clone(), func_idx);

            Ok(Function {
                type_: function.type_,
                body: wimplify_function_body(
                    function,
                    func_idx,
                    module,
                    &mut metadata,
                    &func_orig_idx_to_id_map,
                )?,
                name,
                export: function.export.clone(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let wimpl_table = if let Some(table) = module.tables.first().clone() {
        // Translate function indices to function IDs in table/elem section.
        let mut elements = Vec::new();
        for elem in &table.elements {
            let offset = (*elem.offset).to_vec();
            let mut functions = Vec::new();
            for func_idx in &elem.functions {
                functions.push(func_orig_idx_to_id_map[func_idx.to_usize()].clone());
            }
            elements.push(Element { offset, functions })
        }
        Some(Table {
            limits: table.limits.clone(),
            import: table.import.clone(),
            elements,
            export: table.export.clone(),
        })
    } else {
        None
    };

    Ok(Module {
        functions,

        // TODO translate global init expr and memory offsets to Wimpl also.
        globals: module.globals.clone(),

        table: wimpl_table,

        // TODO add (a single) memory.
        metadata,
    })
}

// TODO @Dmitrii abstract dewimplify into a separate module

// translate a given wimpl expression into a vector of wasm instructions
fn dewimplify_expr(
    expr: Expr,
    params_num: u32,
    type_: Option<ValType>,
    result_stack: &mut HashMap<u32, Vec<wasm::Instr>>,
) -> Vec<wasm::Instr> {
    let mut instrs = Vec::new();
    match expr.kind {
        ExprKind::Const(val) => instrs.push(wasm::Instr::Const(val)),
        // match the types of VarRef
        ExprKind::VarRef(v) => {
            match v {
                Var::Local(idx) => {
                    // if the local is referenced, get its value
                    instrs.push(wasm::Instr::Local(
                        wasm::LocalOp::Get,
                        // the index of the local is offset from the number of parameters
                        wasm::Idx::from(params_num + idx),
                    ));
                }
                Var::Global(_) => todo!(),
                Var::Stack(_) => (),
                // update the function param set if the referenced param is first encountered
                Var::Param(idx) => {
                    instrs.push(wasm::Instr::Local(
                        wasm::LocalOp::Get,
                        // for params, index is as is
                        wasm::Idx::from(idx),
                    ));
                }
                Var::BlockResult(label) => {
                    result_stack.remove(&label); // when the blockresult is referenced, it's no longer on the stack
                }
                Var::Return(_) => (),
            }
        }
        ExprKind::Load { op, addr } => todo!(),
        ExprKind::MemorySize => todo!(),
        ExprKind::MemoryGrow { pages } => todo!(),
        ExprKind::Unary(op, e) => {
            for i in dewimplify_expr(*e, params_num, type_, result_stack) {
                instrs.push(i);
            }
            instrs.push(wasm::Instr::Unary(op));
        }
        ExprKind::Binary(op, ex1, ex2) => {
            // hint @Dmitrii : * is for unboxing (dereferencing) a heap pointer
            // unroll the result of the expression into the vector of instructions
            for instr in dewimplify_expr(*ex1, params_num, type_, result_stack) {
                instrs.push(instr)
            }
            for instr in dewimplify_expr(*ex2, params_num, type_, result_stack) {
                instrs.push(instr)
            }
            instrs.push(wasm::Instr::Binary(op));
        }
        ExprKind::Call { func, args } => (),
        ExprKind::CallIndirect {
            type_,
            table_idx,
            args,
        } => todo!(),
    }
    instrs
}

// FIXME @Dmitrii: where should I define this struct? In the global scope or somewhere locally?
struct BlockFrame {
    label: Label,           // label of the block, as represented in wimpl
    result_type: BlockType, // a type of the result of the given block of code in the function (can be VarType or a None)
    is_if: bool,            // is this block a wrapper around an if_else block?
}

// translate a given wimpl statement into a vector of wasm instructions
fn dewimplify_stmt(
    stmt: Stmt,
    params_num: u32,
    locals: &mut HashMap<u32, Local>,
    block_stack: &mut Vec<BlockFrame>,
    result_stack: &mut HashMap<u32, Vec<wasm::Instr>>,
) -> Vec<wasm::Instr> {
    let mut instrs = Vec::new();

    match stmt.kind {
        // hint @Dmitrii : :: is the path separator
        // hint @Dmitrii : () is the destructuring syntax
        StmtKind::Unreachable => todo!(),
        // process all expressions with dewimplify_expr
        StmtKind::Expr(e) => {
            instrs.extend(dewimplify_expr(e, params_num, None, result_stack));
            instrs.push(wasm::Instr::Drop);
        }
        StmtKind::Assign { lhs, type_, rhs } => {
            // init the vector of righthand instructions
            let mut rhs_instr = Vec::new();

            // process the righthand side expression
            for i in dewimplify_expr(rhs, params_num, Some(type_), result_stack) {
                rhs_instr.push(i);
            }

            match lhs {
                // process local variable assignments
                Var::Local(idx) => {
                    instrs.extend(rhs_instr); // append the righthand side instructions to the list
                    if locals.insert(
                        // insert a new local variable with a given type and id to a list of locals
                        idx,
                        Local { type_, name: None },
                    ) == None
                    {
                        // if there was no local with the given id in the map
                        // delete the last pushed instr if it was just an initialization of a local
                        instrs.truncate(instrs.len() - 1);
                    } else {
                        // else, the local already existed in the list of locals, so set its value
                        instrs.push(wasm::Instr::Local(
                            wasm::LocalOp::Set,
                            wasm::Idx::from(params_num + idx), // the index of the local is offset from the number of parameters
                        ));
                    };
                }
                // ??? @Dmitrii I assume this works similar to locals except the param counting part
                Var::Global(_) => todo!(),
                Var::Stack(_) => {
                    instrs.extend(rhs_instr); // append the righthand side instructions to the list
                }
                // update the function param map if the referenced param is first encountered
                Var::Param(idx) => {
                    instrs.extend(rhs_instr); // append the righthand side instructions to the list
                                              // for params, always set their value
                    instrs.push(wasm::Instr::Local(
                        wasm::LocalOp::Set,
                        // for params, the index is as is
                        wasm::Idx::from(idx),
                    ));
                }
                // process the results of the block
                Var::BlockResult(label) => {
                    let frame = block_stack
                        .into_iter()
                        .find(|f| f.label.0 == label) // find the frame of the block with the label of the result
                        .expect("Expected a block with the given label to be on the stack!");
                    frame.result_type = BlockType(Some(type_)); // set the result type of the block to the type defined within the Assign stmt

                    if block_stack
                        .last()
                        .expect("Expected the stack to be non-empty!")
                        .label
                        .0
                        == label
                    // && !block_stack
                    //     .last_mut()
                    //     .expect("Expected the stack to be non-empty")
                    //     .is_if
                    // FIXME @Dmitrii probably an is_if check is redundant
                    // if label of the top of the stack matches the assignee label and the block has _not_ been marked as `if` (if it was, we are in an else condition )
                    {
                        if let Some(vec) = result_stack.get_mut(&label) {
                            // if stack of results already contains a result for this block
                            if *vec == rhs_instr {
                                // and it's equal to value assigned here
                                return instrs; // then this value is already on wasm stack and we don't produce any new instructions
                            } else {
                                *vec = rhs_instr.to_vec(); // otherwise we replace the result
                            }
                        } else {
                            // if the result is new and not on the stack of results
                            result_stack.insert(label, rhs_instr.to_vec()); // add it to the result stack
                        }
                    };

                    instrs.extend(rhs_instr);
                }
                Var::Return(idx) => {
                    for _i in 0..result_stack.len() {
                        // add as many drops before the return result, as there are blockresults left on the stack
                        instrs.push(wasm::Instr::Drop);
                    }
                    result_stack.clear(); // no results left on the stack at this point
                    instrs.extend(rhs_instr); // append the righthand side instructions to the list
                }
            }
        }
        StmtKind::Store { op, addr, value } => todo!(),
        StmtKind::Br { target } => {
            // FIXME @Dmitrii refactor this in a separate function (also used in If for BrIfs)
            // wasm target label index corresponds to the level of block nesting, counting from innermost to outermost block
            let wasm_target = (block_stack.len() - 1)
                - block_stack
                    .into_iter()
                    .position(|f| f.label == target)
                    .expect("Expected a given label to be on the stack!"); // we count the index from the innermost (current) block
            instrs.push(wasm::Instr::Br(wasm::Label::from(wasm_target))); // pass the index as the label value and add the Br instruction with that label to the list
        }
        // FIXME @Dmitrii abstract away the similar logic of block and loop?
        // process statements inside the block body
        StmtKind::Block { body, end_label } => {
            let mut frame = BlockFrame {
                // init a block frame for this block
                label: end_label,
                result_type: BlockType(None), // by default the result is None
                // FIXME @Dmitrii delete if redundant
                //results: Vec::new(),          // and the list of instructions is emtpy
                is_if: false,
            };
            block_stack.push(frame); // add a block frame on the stack

            let mut block_instr = Vec::new(); // init a list of instruction within the block
            for stmt in body.0 {
                // process the instructions
                block_instr.extend(dewimplify_stmt(
                    stmt,
                    params_num,
                    locals,
                    block_stack,
                    result_stack,
                ));
            }

            let frame = block_stack
                .last()
                .expect("Expected the stack to be non-empty");

            // if the last frame has an `is_if` flag, then the block is a wrapper for if_else and doesn't need an instruction produced
            if !frame.is_if {
                instrs.push(wasm::Instr::Block(frame.result_type));
            }

            // FIXME @Dmitrii delete if redundant
            // if the frame is not on the stack, it means it was a wrapper around the if_else statement, and was popped during the processing of if_else
            // if let Some(frame) = block_stack.into_iter().find(|f| f.label == end_label) {
            //     instrs.push(wasm::Instr::Block(frame.result_type));
            //     instrs.extend(frame.results.to_vec()); // add all block results to the list of statement instructions
            //     block_stack.pop(); // pop the frame off the stack when exiting the block
            // }

            // FIXME @Dmitrii this might be redundant - check that there are actually any instructions in block body that are not present in the block frame's results
            // add all block instructions to the list of statement instructions
            instrs.extend(block_instr);

            // every block ends with and End
            instrs.push(wasm::Instr::End);

            block_stack.pop(); // pop the frame off the stack when exiting the block
        }
        // FIXME @Dmitrii abstract away the similar logic of block and loop?
        StmtKind::Loop { begin_label, body } => {
            let mut frame = BlockFrame {
                // init a block frame for this loop
                label: begin_label,
                result_type: BlockType(None), // by default the result is None
                // FIXME @Dmitrii delete if redundant
                //results: Vec::new(),          // and the list of instructions is emtpy
                is_if: false,
            };
            block_stack.push(frame); // add a loop frame on the stack

            let mut loop_instr = Vec::new(); // init a list of instruction within the loop
            for stmt in body.0 {
                // process the instructions
                loop_instr.extend(dewimplify_stmt(
                    stmt,
                    params_num,
                    locals,
                    block_stack,
                    result_stack,
                ));
            }

            let frame = block_stack
                .last()
                .expect("Expected the stack to be non-empty");

            instrs.push(wasm::Instr::Loop(frame.result_type)); // push the Loop instruction
            instrs.extend(loop_instr); // push instruction from inside the loop
            instrs.push(wasm::Instr::End); // every loop ends with and End

            block_stack.pop(); // pop the frame off the stack when exiting the block
        }
        StmtKind::If {
            condition,
            if_body,
            else_body,
        } => {
            let mut conditions = Vec::new(); // process instructions for the if condition into a separate vector
            conditions.extend(dewimplify_expr(condition, params_num, None, result_stack));

            if let Some(stmt) = if_body.0.last() {
                if let StmtKind::Br { target } = stmt.kind {
                    // check is the last stmt in the block is a break
                    let mut body = if_body.0.to_vec(); // copy the if body
                    block_stack
                        .last_mut()
                        .expect("Expected the block stack to be non-empty!")
                        .is_if = true; // set flag is_if to true to not count the blockresult within the block with br_if condition
                    body.pop().expect("Expected there to a BrIf statement!"); // pop the break stmt from the if body
                    for stmt in body {
                        instrs.extend(dewimplify_stmt(
                            stmt,
                            params_num,
                            locals,
                            block_stack,
                            result_stack,
                        ));
                    } // process the rest of the stmts
                    instrs.extend(conditions); // push conditions

                    // FIXME @Dmitrii refactor this in a separate function (also used in Br)
                    let wasm_target = (block_stack.len() - 1) // wasm target label index corresponds to the level of block nesting, counting from innermost to outermost block
                        - block_stack
                            .iter_mut()
                            .position(|f| f.label == target)
                            .expect("Expected a given label to be on the stack!"); // we count the index from the innermost (current) block
                    instrs.push(wasm::Instr::BrIf(wasm::Label::from(wasm_target))); // pass the index as the label value and add the Br instruction with that label to the list
                    block_stack
                        .last_mut()
                        .expect("Expected the block stack to be non-empty!")
                        .is_if = false; // set flag is_if to false since for br_ifs blocks are not just wrappers
                    return instrs; // processing for if block is done here
                }
            }

            // process the rest of the stmts
            instrs.extend(conditions); // push conditions

            let mut then_instrs = Vec::new();
            // first process a then body to get a result type for the block frame
            for stmt in if_body.0 {
                then_instrs.extend(dewimplify_stmt(
                    stmt,
                    params_num,
                    locals,
                    block_stack,
                    result_stack,
                ));
            }
            instrs.push(wasm::Instr::If(
                // push IF with Blocktype
                block_stack
                    .last() // peek at the top of the stack
                    .expect("Expected the stack to be non-empty!")
                    .result_type,
            ));
            instrs.extend(then_instrs); // add the then block instructions

            // mark the top block on the stack as an if_else wrapper block
            block_stack
                .last_mut()
                .expect("Expected the stack to be non-empty")
                .is_if = true;

            // FIXME @Dmitrii delete if redundant
            // instrs.extend(
            //     block_stack
            //         .last()
            //         .expect("Expected a block wrapper for if_else to be on the stack!")
            //         .results
            //         .to_vec(),
            // ); // add the if result instructions
            if let Some(else_body) = else_body {
                // add the else instruction if the body is not empty
                if !else_body.0.is_empty() {
                    instrs.push(wasm::Instr::Else);
                }
                // process instructions for the else condition
                for stmt in else_body.0 {
                    instrs.extend(dewimplify_stmt(
                        stmt,
                        params_num,
                        locals,
                        block_stack,
                        result_stack,
                    ));
                }
                // FIXME @Dmitrii delete if redundant
                // instrs.extend(
                //     block_stack
                //         .last()
                //         .expect("Expected a block wrapper for if_else to be on the stack!")
                //         .results
                //         .to_vec(),
                // ); // add the else result instructions
            }

            // FIXME @Dmitrii delete if redundant
            //block_stack.pop(); // the last block on the stack is the wrapper around the if statement, so pop it
        }
        StmtKind::Switch {
            index,
            cases,
            default,
        } => (),
    }
    return instrs;
}

// TODO @Dmitrii translate and dewimplify function
// a vector of initialized locals is added here

#[test]
fn dewimplify_with_expected_output() {
    use std::path::PathBuf;
    use walkdir::WalkDir;

    // FIXME @Dmitrii change back to parent directory
    // define a path to the parent directory with all test directories and files
    const DEWIMPL_TEST_INPUTS_DIR: &'static str = "tests/dewimplify_expected/br_table";

    // Sort for deterministic order.
    let mut files: Vec<PathBuf> = WalkDir::new(&DEWIMPL_TEST_INPUTS_DIR)
        .into_iter()
        .map(|entry| entry.unwrap().path().to_owned())
        .collect();
    files.sort();

    // FIXME @Dmitrii delete debug
    // counter for the number of ast-to-ast comparisons passed
    let mut ast_tests = 0;

    for wasm_path in files {
        // FIXME @Dmitrii added this for debug to target a specific file name for testing
        // if let Some("local") = wasm_path.file_stem().and_then(|os_str| os_str.to_str()) {

        // Find all files, where a <name>.wasm file and a <name>.wimpl file are next to each other.
        if let Some("wasm") = wasm_path.extension().and_then(|os_str| os_str.to_str()) {
            let wimpl_path = wasm_path.with_extension("wimpl");

            if wimpl_path.exists() {
                // Parse the WASM file into an AST.
                let module = wasm::Module::from_file(&wasm_path).unwrap_or_else(|_| {
                    panic!("could not decode valid wasm file '{}'", wasm_path.display())
                });

                // get the vector of instructions from the wasm module
                let instrs_from_wasm = module.0.functions[0].instrs();
                // FIXME @Dmitrii make more idiomatic without unwrap?
                // .locals for functions[i] won't help since it returns an <Idx, Local> iterator, where Idx has the true Idx of the local (= local_idx + param_count) – not the index of the local in wasm ast
                // get the vector of locals from the wasm module
                // copying it into a vector since otherwise it has to be borrowed
                let locals_from_wasm = module.0.functions[0].code().unwrap().locals.to_vec();
                // get the wasm function type
                let ftype_wasm = module.0.functions[0].type_;
                let params_num = ftype_wasm.inputs().len() as u32;

                // FIXME @Dmitrii delete debug
                println!("\n{:?}", wimpl_path);

                // Parse the Wimpl file into an AST.
                let stmts =
                    Stmt::from_text_file(&wimpl_path).expect("could not parse Wimpl text file");

                // FIXME @Dmitrii delete debug
                println!("\n WIMPL STMTS FROM WIMPL:\n {:#?}", stmts);

                // init the stack of blocks that dewimplify recurses into during translation
                let mut block_stack = Vec::new();

                // init the list of locals acquired from wimpl <usize idx, Local loc>
                let mut locals_from_wimpl = HashMap::new();

                // Initialize a vector of Instructions
                let mut instrs_from_wimpl = Vec::new();

                // Initialize a block result counter - it is used to compare with the number of returns of the function and drop values that are not returned
                let mut result_stack = HashMap::new();

                // Push the dewimplified instructions into the vector
                for stmt in stmts.iter().cloned() {
                    // TODO @Dmitrii change processing here to get other parts of the module besides the instructions
                    let instrs = dewimplify_stmt(
                        stmt,
                        params_num,
                        &mut locals_from_wimpl,
                        &mut block_stack,
                        &mut result_stack,
                    );
                    // unroll the result of the dewimplification into the vector of instructions
                    instrs_from_wimpl.extend(instrs);
                }

                // FIXME @Dmitrii change that  when implementing the translation of several functions
                for _i in 0..result_stack.len() {
                    // add drops if there are still results left on the stack
                    instrs_from_wimpl.push(wasm::Instr::Drop);
                }
                // FIXME @Dmitrii change that  when implementing the translation of several functions
                instrs_from_wimpl.push(wasm::Instr::End);

                // FIXME @Dmitrii can I make this more idiomatic?
                // convert the hashmap into a vector of locals
                let mut temp = Vec::new();
                temp = locals_from_wimpl.iter().collect();
                temp.sort_by(|a, b| (*a).0.cmp((*b).0));
                // redifine the type and value of locals_from_wimpl
                let locals_from_wimpl = temp.iter().map(|t| (*t).1.to_owned()).collect();

                // FIXME @Dmitrii parse ftype from wimpl module when implemented
                // define the function type
                // let ftype = wasm::FunctionType::new(&param_types, &return_types);

                // FIXME @Dmitrii parse ftype from wimpl module when implemented
                // checking that function type is the same
                // assert_eq!(
                //     ftype_wasm,
                //     ftype,
                //     //FIXME @Dmitrii get rid of uppercase
                //     "\n FUNCTION TYPE from \n{} \nisn't the same as from \n{}\n",
                //     wasm_path.display(),
                //     wimpl_path.display()
                // );

                // FIXME @Dmitrii delete as redundant???
                // checking that function locals are the same
                // assert_eq!(
                //     locals_from_wasm,
                //     locals_from_wimpl,
                //     //FIXME @Dmitrii get rid of uppercase
                //     "\nLOCALS from \n{} \naren't the same as from \n{}\n",
                //     wasm_path.display(),
                //     wimpl_path.display()
                // );

                // FIXME ? @Dmitrii not removing files to have result files for inspection
                // delete a temporary test file
                // remove_file(p);

                // FIXME @Dmitrii delete debug
                println!(
                    "\n WIMPL MODULE FROM WASM {:#?}",
                    Module::from_wasm_file(&wasm_path)
                );

                // FIXME @Dmitrii delete debug
                println!("\n OG WASM MODULE FROM WASM {:#?}\n", module);

                // FIXME @Dmitrii debug - delete
                // print!("\n NEW WASM MODULE FROM WIMPL: {:?}\n", m);

                // FIXME @Dmitrii get rid of if checking through wasmtime?
                // checking that function instructions are the same
                assert_eq!(
                    instrs_from_wasm,
                    instrs_from_wimpl,
                    //FIXME @Dmitrii get rid of uppercase
                    "\nINSTRUCTIONS from {} aren't the same as from {}\n",
                    wasm_path.display(),
                    wimpl_path.display()
                );

                // TODO @Dmitrii abstract into a utility function?
                // store a vector of instructions into a WASM Module
                // init a wasm module
                let mut m = wasm::Module::new();
                // add functions to the module
                m.functions = vec![wasm::Function::new(
                    // currently supports only one function
                    ftype_wasm, // FIXME @Dmitrii parse ftype from wimpl module when implemented
                    wasm::Code::new(),
                    vec!["test".to_string()], // exported name of the function
                )];
                // provide instructions vector generated from wimpl as a body of the function code
                m.functions[0]
                    .code_mut()
                    .expect("No code present in this function!")
                    .body = instrs_from_wimpl;
                // provide locals vector generated from wimpl as a locals property of the function
                m.functions[0]
                    .code_mut()
                    .expect("No code present in this function!")
                    .locals = locals_from_wimpl;

                // copy wasm path
                let mut p = wasm_path.to_path_buf();
                // extract the part of the file name string before the extension
                let mut fl = p.file_stem().expect("Expect a file name").to_os_string();
                // modify the file name string with `_temp_' and an extension
                fl.push("_temp.wasm");
                // set new file name
                p.set_file_name(fl);

                // write binary to a file at the path given above
                m.to_file(&p)
                    .expect("Expected a write to commence correctly!");

                // check that the validation is successful
                assert!(test_utilities::wasm_validate(&p.as_path()) == Ok(())); // wat2wasm has to be on the path

                // test that the original wasm binary and the regenerated one evaluate to the same thing
                use std::process::Command; // import a process builder

                let mut run_wasm = Command::new("wasmtime"); // execute wasmtime command
                                                             // wasmtime has to be on the path

                // TODO @Dmitrii: to avoid copying code, can refactor this as a function with path string as a parameter
                let og_output = run_wasm
                    .arg(wasm_path.as_os_str()) // full path to the original wasm file
                    .arg("--invoke") // invoke the provided function within the module
                    .arg("test") // export name of the function to invoke
                    .args(["10", "20"]) // function arguments
                    // currently support only two arguments
                    .output()
                    .expect("wasmtime failed to execute");

                let new_output = run_wasm
                    .arg(p.as_os_str()) // full path to the regenerated wasm file
                    .arg(wasm_path.as_os_str()) // full path to the original wasm file
                    .arg("--invoke") // invoke the provided function within the module
                    .arg("test") // export name of the function to invoke
                    .args(["10", "20"]) // function arguments
                    // currently support only two arguments
                    .output()
                    .expect("wasmtime failed to execute");

                assert_eq!(
                    og_output, new_output,
                    "\nExpected \n{:?} \nand \n{:?} to be equal\n",
                    og_output, new_output
                );

                // FIXME @Dmitrii debug delete
                println!("\n OUTPUT: {:?}", og_output);

                ast_tests += 1;
            }
        }
        // }
    }

    // FIXME @Dmitrii delete debug
    print!("\nASTs compared: {}\n", ast_tests);
}
