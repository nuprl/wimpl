//! Conversion from Wimpl back to standard WebAssembly

use wasabi_wasm as wasm;
use wasm::Local;

use crate::*;

// translate a given wimpl expression into a vector of wasm instructions
fn dewimplify_expr(
    expr: Expr,
    params_num: u32,
    result_map: &mut HashMap<u32, Vec<wasm::Instr>>,
    id_map: &HashMap<FunctionId, wasm::Idx<wasm::Function>>,
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
                    result_map.remove(&label); // when the blockresult is referenced, it's no longer on the stack
                }
                Var::Return(_) => (),
            }
        }
        ExprKind::Load { op, addr } => todo!(),
        ExprKind::MemorySize => todo!(),
        ExprKind::MemoryGrow { pages } => todo!(),
        ExprKind::Unary(op, e) => {
            for i in dewimplify_expr(*e, params_num, result_map, id_map) {
                instrs.push(i);
            }
            instrs.push(wasm::Instr::Unary(op));
        }
        ExprKind::Binary(op, ex1, ex2) => {
            // unroll the result of the expression into the vector of instructions
            instrs.extend(dewimplify_expr(*ex1, params_num, result_map, id_map));
            instrs.extend(dewimplify_expr(*ex2, params_num, result_map, id_map));
            instrs.push(wasm::Instr::Binary(op));
        }
        ExprKind::Call { func, args } => {
            for a in args {
                instrs.extend(dewimplify_expr(a, params_num, result_map, id_map));
            }
            let temp = FunctionId::Idx(2);
            instrs.push(wasm::Instr::Call(
                *id_map.get(&func).expect("expected a function idx"), // pass function index from the map as a call parameter
            ));
        }
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
    result_map: &mut HashMap<u32, Vec<wasm::Instr>>,
    id_map: &HashMap<FunctionId, wasm::Idx<wasm::Function>>,
) -> Vec<wasm::Instr> {
    let mut instrs = Vec::new();

    match stmt.kind {
        StmtKind::Unreachable => todo!(),
        // process all expressions with dewimplify_expr
        StmtKind::Expr(e) => {
            instrs.extend(dewimplify_expr(e, params_num, result_map, id_map));
            instrs.push(wasm::Instr::Drop);
        }
        StmtKind::Assign { lhs, type_, rhs } => {
            // init the vector of righthand instructions
            let mut rhs_instr = Vec::new();

            // process the righthand side expression
            for i in dewimplify_expr(rhs, params_num, result_map, id_map) {
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
                        .iter_mut()
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
                        if let Some(vec) = result_map.get_mut(&label) {
                            // if stack of results already contains a result for this block
                            if *vec == rhs_instr {
                                // and it's equal to value assigned here
                                return instrs; // then this value is already on wasm stack and we don't produce any new instructions
                            } else {
                                *vec = rhs_instr.to_vec(); // otherwise we replace the result
                            }
                        } else {
                            // if the result is new and not on the stack of results
                            result_map.insert(label, rhs_instr.to_vec()); // add it to the result stack
                        }
                    };

                    instrs.extend(rhs_instr);
                }
                Var::Return(idx) => {
                    for _i in 0..result_map.len() {
                        // add as many drops before the return result, as there are blockresults left on the stack
                        instrs.push(wasm::Instr::Drop);
                    }
                    result_map.clear(); // no results left on the stack at this point
                    instrs.extend(rhs_instr); // append the righthand side instructions to the list
                }
            }
        }
        StmtKind::Store { op, addr, value } => todo!(),
        StmtKind::Br { target } => {
            // FIXME @Dmitrii refactor this in a separate function (also used in If for BrIfs)
            // let wasm_target = block_stack.len() - target.0 as usize; // wasm target label index corresponds to the level of block nesting, counting from innermost to outermost block

            let wasm_target = (block_stack.len() - 1)
                - block_stack
                    .iter_mut()
                    .position(|f| f.label == target)
                    .expect("Expected a given label to be on the stack!"); // we count the index from the innermost (current) block

            instrs.push(wasm::Instr::Br(wasm::Label::from(wasm_target))); // pass the index as the label value and add the Br instruction with that label to the list
        }
        // FIXME @Dmitrii abstract away the similar logic of block and loop?
        // process statements inside the block body
        StmtKind::Block { body, end_label } => {
            let frame = BlockFrame {
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
                    result_map,
                    id_map,
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
            let frame = BlockFrame {
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
                    result_map,
                    id_map,
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
            conditions.extend(dewimplify_expr(condition, params_num, result_map, id_map));

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
                            result_map,
                            id_map,
                        ));
                    } // process the rest of the stmts
                    instrs.extend(conditions); // push conditions

                    // FIXME @Dmitrii refactor this in a separate function (also used in Br)
                    // let wasm_target = block_stack.len() - target.0 as usize; // wasm target label index corresponds to the level of block nesting, counting from innermost to outermost block

                    let wasm_target = (block_stack.len() - 1)
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
                    result_map,
                    id_map,
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
                        result_map,
                        id_map,
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
            // block_stack.pop(); // the last block on the stack is the wrapper around the if statement, so pop it
        }
        StmtKind::Switch {
            index,
            cases,
            default,
        } => {
            let mut switch_results = result_map.clone();
            let mut process_body = |mut b: Body| {
                let mut body_results = result_map.clone();
                let mut body_labels = Vec::new();
                // closure for processing body of stmts
                let mut body_instr = Vec::new();
                if let Some(StmtKind::Br { target }) = b.0.last().map(|s| s.to_owned().kind)
                // take last statement of each switch case (should be a Br statement) and extract target label
                {
                    // FIXME @Dmitrii refactor this in a separate function (also used in Br)
                    // let wasm_target = block_stack.len() - target.0 as usize; // wasm target label index corresponds to the level of block nesting, counting from innermost to outermost block

                    let wasm_target = (block_stack.len() - 1)
                        - block_stack
                            .iter_mut()
                            .position(|f| f.label == target)
                            .expect("Expected a given label to be on the stack!"); // we count the index from the innermost (current) block

                    body_labels.push(wasm::Label::from(wasm_target)); // add label to the vector of labels
                    b.0.pop(); // remove Br from the statements stack
                }
                for s in b.0 {
                    // run the dewimpl_stmt w/o using the output (to update the stack and the map)
                    body_instr.extend(dewimplify_stmt(
                        s,
                        params_num,
                        locals,
                        block_stack,
                        &mut body_results,
                        id_map,
                    ));
                }
                switch_results.extend(body_results);
                (body_instr, body_labels)
            };

            let mut cases_labels = Vec::new(); // init a vector of labels used in the br_table
            for b in cases {
                cases_labels.extend(process_body(b).1);
            }
            let (mut switch_instr, default_label) = process_body(default); // process the `default`
            result_map.extend(switch_results); // add all unique switch results to the main results map
            switch_instr.extend(dewimplify_expr(index, params_num, result_map, id_map)); // process and push `index` to the stack
            switch_instr.push(wasm::Instr::BrTable {
                table: cases_labels,
                default: default_label[0],
            });
            instrs.extend(switch_instr);
        }
    }
    instrs
}

fn dewimplify_function(
    wasm_f: wasm::Function,
    wimpl_f: Function,
    id_map: &HashMap<FunctionId, wasm::Idx<wasm::Function>>,
) -> wasm::Function {
    // get the vector of instructions from the wasm module
    let wasm_instr = wasm_f.instrs();
    // FIXME @Dmitrii make more idiomatic without unwrap?
    // .locals for functions[i] won't help since it returns an <Idx, Local> iterator, where Idx has the true Idx of the local (= local_idx + param_count) – not the index of the local in wasm ast
    // get the vector of locals from the wasm module
    // copying it into a vector since otherwise it has to be borrowed
    // let locals_from_wasm = module.0.functions[0].code().unwrap().locals.to_vec();
    // get the wasm function type
    let ftype_wasm = wasm_f.type_;
    let params_num = ftype_wasm.inputs().len() as u32;

    let wimpl_stmts = wimpl_f.body.expect("expected a function body").0;

    // FIXME @Dmitrii delete debug
    println!("\n WIMPL STMTS FROM WIMPL:\n {:#?}", wimpl_stmts); // FIXME here from stmt:from_text_file before

    // init the stack of blocks that dewimplify recurses into during translation
    let mut block_stack = vec![
        // add the start of the function as a frame (to be able to refer to the start of the function as the Br target label)
        BlockFrame {
            label: Label(0),
            result_type: BlockType(None), // by default the result is None
            is_if: false,
        },
    ];

    // init the list of locals acquired from wimpl <usize idx, Local loc>
    let mut locals_from_wimpl = HashMap::new();

    // Initialize a vector of Instructions
    let mut instrs_from_wimpl = Vec::new();

    // Initialize a block result counter - it is used to compare with the number of returns of the function and drop values that are not returned
    let mut result_map = HashMap::new();

    // Push the dewimplified instructions into the vector
    for stmt in wimpl_stmts.iter().cloned() {
        // TODO @Dmitrii change processing here to get other parts of the module besides the instructions
        let instrs = dewimplify_stmt(
            stmt,
            params_num,
            &mut locals_from_wimpl,
            &mut block_stack,
            &mut result_map,
            id_map,
        );
        // unroll the result of the dewimplification into the vector of instructions
        instrs_from_wimpl.extend(instrs);
    }

    // FIXME @Dmitrii change that  when implementing the translation of several functions
    for _i in 0..result_map.len() {
        // add drops if there are still results left on the stack
        instrs_from_wimpl.push(wasm::Instr::Drop);
    }
    // FIXME @Dmitrii delete debug
    // FIXME @Dmitrii change that  when implementing the translation of several functions
    instrs_from_wimpl.push(wasm::Instr::End);

    // FIXME @Dmitrii can I make this more idiomatic?
    // convert the hashmap into a vector of locals
    let mut temp: Vec<(&u32, &Local)> = locals_from_wimpl.iter().collect();
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
    //     "\nLOCALS from naren't the same as from ",
    // );

    // FIXME @Dmitrii delete debug
    print!("/n 🔥 BlockFrame length is {}/n", block_stack.len());

    // FIXME @Dmitrii get rid of if checking through wasmtime?
    // checking that function instructions are the same
    assert_eq!(
        wasm_instr, instrs_from_wimpl,
        //FIXME @Dmitrii get rid of uppercase
        "\nINSTRUCTIONS from wasm aren't the same as from wimpl\n",
    );

    let mut function = wasm::Function::new(
        // currently supports only one function
        ftype_wasm, // FIXME @Dmitrii parse ftype from wimpl module when implemented
        wasm::Code::new(),
        vec!["test".to_string()], // exported name of the function
    );

    // provide instructions vector generated from wimpl as a body of the function code
    function
        .code_mut()
        .expect("No code present in this function!")
        .body = instrs_from_wimpl;

    // provide locals vector generated from wimpl as a locals property of the function
    function
        .code_mut()
        .expect("No code present in this function!")
        .locals = locals_from_wimpl;

    function
}

fn dewimplify_module(wasm_mod: wasm::Module, wimpl_mod: Module) -> wasm::Module {
    use std::iter::zip;

    // FIXME ? @Dmitrii not removing files to have result files for inspection
    // delete a temporary test file
    // remove_file(p);

    // FIXME @Dmitrii debug - delete
    // print!("\n NEW WASM MODULE FROM WIMPL: {:?}\n", m);

    // TODO @Dmitrii abstract into a utility function?
    // store a vector of instructions into a WASM Module

    let metadata = wimpl_mod.metadata; // extract metadata

    let functions = zip(wasm_mod.functions, wimpl_mod.functions); // zip wasm functions and wimpl functions together

    let mut new_mod = wasm::Module::new(); // init a wasm module

    for (wasm_f, wimpl_f) in functions {
        new_mod.functions.push(dewimplify_function(
            wasm_f,
            wimpl_f,
            &metadata.func_id_to_orig_idx_map,
        ));
    } // add functions to the module

    new_mod
}

#[test]
fn dewimplify_with_expected_output() {
    // TODO separate tests into dew with exp out?
    // [ ] test first with call tests
    // [ ] -> then rework other tests for proper module code

    use std::path::PathBuf;
    use walkdir::WalkDir;

    // FIXME @Dmitrii change back to parent directory
    // define a path to the parent directory with all test directories and files
    const DEWIMPL_TEST_INPUTS_DIR: &str = "tests/dewimplify_expected/br_table";

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
                let (og_mod, _, _) = wasm::Module::from_file(&wasm_path).unwrap_or_else(|_| {
                    panic!("could not decode valid wasm file '{}'", wasm_path.display())
                });

                // FIXME @Dmitrii delete debug
                println!(
                    "\n WIMPL MODULE FROM WASM {:#?}",
                    Module::from_wasm_file(&wasm_path)
                );

                // FIXME @Dmitrii delete debug
                println!("\n OG WASM MODULE FROM WASM {:#?}\n", og_mod);

                // FIXME @Dmitrii delete debug
                println!("\n{:?}", wimpl_path);

                // Parse the Wimpl file into an AST.
                let wimpl_mod =
                    Module::from_text_file(&wimpl_path).expect("could not parse Wimpl text file");

                let dewimp_wasm = dewimplify_module(og_mod, wimpl_mod);

                // copy wasm path
                let mut p = wasm_path.to_path_buf();
                // extract the part of the file name string before the extension
                let mut fl = p.file_stem().expect("Expect a file name").to_os_string();
                // modify the file name string with `_temp_' and an extension
                fl.push("_temp.wasm");
                // set new file name
                p.set_file_name(fl);

                // write binary to a file at the path given above
                dewimp_wasm
                    .to_file(&p)
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
