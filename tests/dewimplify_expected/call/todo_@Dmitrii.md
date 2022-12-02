// FIXME @Dmitrii delete this file

=== Week 17.10 ===

~~// [V] gotta compare locals now in my tests (not just instructions)~~
~~// [V] set up module => functions[0] => code => locals and functions~~~~[V] => code => body from wimpl module => functions[0] => body~~

~~// [V] count the number of params~~
    ~~// - collect a params set in a *** separate pass ***~~
        ~~// - then pass the set to dewimpl statements fnc~~

~~// [V] pass locals as a param to dewimpl statements~~
~~// [V] pass returns as a param to dewimpl statements~~

~~// [V] process assigns~~

        ~~// [V] if lhs == local && local is not in locals~~
            ~~// vector of locals has to be set IN THE ORDER IT WAS DECLARED~~
~~            ~~// - add it to locals~~
~~                ~~// - get __only the type__~~
~~                    ~~// from Assign parameters~~
~~        ~~// [V] else~~
~~            ~~// parse the rhs: ~~
~~                ~~// VarRef => local.get (using the exact numbering for params and params+offset fo~~r locals) 
~~                    ~~// if VarRef to Stack => do nothing~~
~~            ~~// parse the lhs => local.set (using the exact numbering for params and params+offset for locals)~~

~~        ~~// [V] stack locals => just translate them as their corresponding rhs?~~
~~            ~~// !! but that will lead to local.set+local.get translation instead of local.tee~~
~~                ~~// **do this as a first iteration**~~
~~                ~~// consider peek() and consume()~~

~~        ~~// [V] returns - just parse the instrs?~~

~~    // [V] store locals in function->code->locals~~

~~// [V] use locals and returns to generate type_~~
~~    // [V] rewrite params to store types~~
~~    // [V] set up returns~~
~~    // [V] gotta compare _type now in my tests (not just instructions)~~
~~    // [V] set up wasm module => functions[0] => type_ from wimpl module => functions[0] => type_~~

=== Week 24.10 ===

~~- [V] wasmtime for one function~~
~~- [X] globals *not implemented in parsing*~~

~~- [X] Stack tests *(Went through all the wimplify test that  use stack vars and also through some of tests.rs, but haven't found anything _immediate_ to test)*~~
  - ~~??? I see how there might appear problems with the stack when we introduce control structures and types such as memorysize, but I don't see how this is a problem with the stack when we're just assigning a value to a variable.~~
  ~~- ??? Unclear how stack references as operands work~~
    ~~- [ ] find if there are test cases where the stack var refers to a value that is already OFF wasm stack (e.g.Test 2 below) and check the corresponding .wat file for whether the value is being put back on the wasm stack~~


~~- [V] Br~~

  ~~- break to a block jumps to the very end of the block~~

  ~~- [X] pass in a vec of labels~~

  ~~- [V] for kind: block -> add label to list of labels AS A FIRST COMMAND~~
  ~~- [V] for kind: block -> pop the stack when we exit a block or a loop AS A LAST COMMAND~~
  
  ~~- [X] for kind: br write a function to count the level of nesting of a block~~
  ~~  - [V] find value -> return index of it~~
  ~~    - wasm numeric index references to blocks work from innermost to outmost => the break to myself is 0, and further out is +1 etc ~~
  ~~- [V] push `Br` instr with `Label(INDEX)`~~


  ~~New concerns:~~

  ~~- [V] what happens with values on the stack in the block that returns nothing? *I assume, it gets popped off the stack implicitly*~~
  ~~  - that is why **in wimpl, when a block has no return value, the return value of that block is assigned to a return value of the block above**~~
  ~~- [X] what happens with values on the stack in the block that returns something and branches? *it gets popped implicity*~~
  ~~- [X] do we have to explicitly drop the value at the end of a block if the block has no result type? *no*~~
  ~~  - [X] or is the value dropped when break occurs? *implicitly by wasm engine, no drop intruction is produced*~~
  ~~- [V] is it a bug or a feature that in  br_nested_simple.wiimp we assign the value of block return after innermost block and not before it? *a feature (tested with wimplify_t~~ests)*
  ~~- [V] and is it a bug that on line 6 we assign the value to b1 whereas in .wat we have innermost block return nothing? *a feature (tested with wimplify_tests)*~~

  ~~- blockresults~~

    ~~- [X] pass aroudn a hashmap of blockresults~~

    ~~- [V] stack of block_frames instead???~~
    ~~  struct block_frame {~~
    ~~    - label: Label,~~
    ~~    - result_type: Blocktype(Option<VarType>)~~
    ~~    - results: Vec<Instr>~~
    ~~  }~~

    ~~- [X] stack of **mutable** frames?~~

   ~~ - in Assign: ~~
   ~~     - [V] push instr in **Vec RHS**, ~~
   ~~     - for Blockresult: ~~
   ~~       - [V] fetch the blockresul from the list **using the label** __ and assign the type_ to it __~~
   ~~       - [V] if **label of the top of the stack** matches the **assignee** label: **append** instr to block_frame.find~~(label).results
   ~~       - [V] else: append to **instrs**~~
   ~~     - [V] for every kind **except Blockresult**, append the Vec RHS to **instrs** _before_ processing the kind itself~~
   
      ~~- [V] in kind: Block, init a blockresult with None~~
      ~~  - [V] as a result_type~~
      ~~  - [V] push the blockresult **before the body**~~

     ~~ - *should be solved by if else condition in Assign Blockresult*~~
     ~~ - how to fetch result type with the if???~~
     ~~   - [V] peek the top frame of the stack and fetch the type of ii~~
     ~~   - ??? do I also fetch **the results** themselves??? or should I just push them before processing the if type???~~

      ~~- [V] test current brs first~~

      ~~- IFs are broken:~~
      ~~- [V] fix the **.extend logic into .replace** for vector of results~~
      ~~  - [V] in Stmt::If pop the closest outer block~~
      ~~  - [V] gotta empty the list of results and use in If~~
      ~~    - [V] fix the comment for blockresult assign (remove the part about the ifs)~~
      ~~  - [V] same for **else**~~
      ~~- [V] in Stmt::Block and if no block found, return from kind:block ?~~

      ~~- ??? How to resolve nested ifs where in .wimpl the result of the if_else statement is assigned to the block above but in .wat the nested if_else is supposed to have a result value? *Reslolve when Michellse implements block results type declarations in wimpl*~~

      ~~- ??? what's going to happen when ifs have no results??? (and are not in blocks???)~~
        ~~- [ ] see other if tests~~


      ~~-[V]  delete param/result collecting for func_type generation, and instead use the func_type from wasm module;~~
      ~~  - [V] count **the number of params**~~
      ~~  - [V] then test whether br tests pass~~
      ~~  - [V] test if locals pass~~
      ~~- [V] test all tests~~

      ~~- [X] what about BlockResults **VarRefs** ???~~

    ~~- [X] are `end`s added correctly at the end of blocks?  ~~
    ~~- ??? do we even have to add breaks if the block ends naturally?~~
    ~~  - most likely not~~

    ~~- [V] 🔥 change test directory back to all of dewimplify~~


=== Week 31.10 ===


~~- [V] fix Drops~~
  ~~- [V] test with wasmtime without stmts comparisons *FAILS*~~
  ~~  - [V] test the original block_nested test => see if wimpl optimizations check out *FAILS*~~
  ~~    - [V] see how and when drops are inserted (wasm from wimpl doesn't have a drop at the end of the function - why?)~~
        ~~- ??? why is there a drop after stmt::expr ?~~
         ~~- *because I was inserting drops after simple Expr stmts like add.wat test (which basically was an end of a function)*~~
        
        ~~- cases of drops:~~
          ~~- end of function if function has no return value~~
              ~~- [X] count result values~~
              ~~    - [X] get it from func_type instead~~
              ~~- [V] how to count how many values left on wasm stack?~~
              ~~  - Are there stmt expe that are NOT dropped? *I assume not since such expres would have been used in s, l, p, or r variables*~~
              ~~  - Stmts that matter for counting stack length~~
              ~~    - Bs~~
              ~~    - Rs~~
              ~~  - [X] Check how Bs are consumed (search for b0 mentions)~~
              ~~  - So is there ever a decrease in result_count happening?~~
              ~~    - when B is reffed inside an operator, an **operator itself** also produces a result!~~
              ~~      - but what to do with B = B assignments?!~~
              ~~        - **!!!** Bs mentions with Expr stmt don’t matter if these stmts are not assigned to Bs or results => **decrease** result count when there is B Ref~~
              ~~    *ASSUMPTION*: there can’t be a reference to a B within a B or R within an R (because neither exists before they are declared)~~
              ~~  - [X] Compare Return count and B count at the end of function dewimpl => add as many drops as there is a difference between the two~~
        
      ~~-[X] How to check if B was consumed??? (VarRef B???)~~
        ~~- Ss dont matter since they are not materialized if **not used anywhere later**~~

            ~~    - [V] where and when to drop those values?~~
        ~~- end of block, if block has not result value~~
        ~~  - ??? but that is probably optimized?~~
        ~~- **drop of variable references is optimized** (such that they don't exist in wimpl code at all)~~
        

        ~~- it seems like the wimpl code for simple expression tests is incorrect *it is correct, we don't use stack variables for simple expressions*~~
          ~~- instead of simple expression statements, wimplify should produce stack variable assignements *no, it shouldn't*~~
            ~~- ??? and then even optimize them away if they are dropped?~~
              ~~- **only const expressions on the stack that are dropped are optimized** (operations with these consts (like add etc) are **not** optimized)~~

      
      ~~- ??? Can there be a case that by dropping values at the very end we are dropping actual returns vs unused values?~~
        ~~- *no, since the returns are the very end of the stack*~~
          ~~- [V] check if returns are ever defined as **not a last value** *they are not*~~
            ~~- [V] if no such returns => add drops before return assignement~~
              ~~- ??? but what about refs within the returns?~~
      
      ~~- [V] Instead, just add as many drops **before the return** as there are block_results left **greater than 0**~~
      - ~~[X] what about stack vars? do we count them as results or not? *no, we don't - only blockresult refs and blockresult assigns matter*~~
        ~~- assign => +1 to count~~
        ~~- refs => -1 to count~~


~~- [X] check with Multiple Returns~~
  ~~- ??? can't test it because multiple values are not supported by wimplify (only WASM MVP is supported)~~


~~- [V] Drop has an error: it is only added when we are encountering a return; this is incorrect, since some functions don't have return values but still have to drop ~~
  ~~- where to push the drop such that it's pushed **before** the returns?~~
    ~~- [V] check where the final End is added - maybe push a drop there~~
    ~~- [V] or maybe push a drop in the case when a function_type indicates no return value~~


~~- [V] Block FAILS~~
~~  - [V] see block_nested~~
~~  - ??? how to fix block result insertion? inserting before the block doesn't work if the nested block has a result that is later used in the outer block~~
~~    - ??? maybe base the result insertion on block result type?~~
~~    - ??? if block has result, push instrs after??~~
~~  - ??? OR just always push instrs **after** the block? (but then why did I change this logic earlier? (something related to block results of nested blocks))~~
~~    - most likely will cause problems with ifs~~

~~- [V] BrIF~~
  ~~- translates in wimpl into an if surrounded by a block~~
    ~~- stmt comparison will probs fail~~
      ~~- use wasmrun to test equivalency with params~~
  ~~- [V] 0.1 process condition~~
  ~~- [V] 0.2 try checking if last stmt in then block is a break~~
    ~~- 1. [V] flag is_if to true~~
    ~~- 2. [V] pop last stmt~~
    ~~- 3. [V] append br_if~~
    ~~- 4. [V] append condition~~
    ~~- 5. [V] process the rest of stmts~~
    ~~- 6. [V] set is_if to false~~

**🔥 Test features bit by bit**

~~- [V] restore br_if~~~
~~- [V] restore br_if_triple_nested~~~
~~- [V] test br~~~
~~- [V] test locals~~~
~~- [V] test blocks~~~
~~- [V] test ifs~~~
~~- [V] test drops~~~

~~- [V] Loops~~
~~    - breeak to a loop jumps to the start of the loop~~
~~    - [V] make a test with br_if to loop~~
~~    - [V] make loop logic~~
~~      - push frame onto the stack in the loop~~

=== Week 7.11 ===

- [V] BrTbl
  ~~- ??? why in br_table.wimpl `@label0` is not assigned to anything in the code? how do we know where label0 is? *label0 actually refers to the function -> br label0 exits the function*~~
  
  ~~How it works:~~
  ~~- value on top of the stack specifies **an index** into the table of br_table~~
    ~~- a table consists of block indeces to which the br jumps~~
      ~~- **last** value specified in the table is **the default block index to break to** (we jump to it when the value on top of the stack is greater than the size_of_the_table - 2)~~
    ~~- if br is not wrapped in a block, then br 0 leads to exiting a function~~
  
  ~~Implementation:~~
  ~~- switch stmt has `index`, `cases` and `default`~~
     ~~- index = condition~~
     ~~- default has a body of stmts~~
     ~~- cases has several bodies of stmts~~

    ~~1. parse body~~
       ~~- `case_body` function???~~
         ~~1. pick the last stmt of the body and parse as br~~
         ~~2. br_tab function passes a vector of indeces~~
         ~~3. br function takes in arg `is_table`~~
          ~~- if `is_table` -> don't process like break, but push label into the index vector~~
         ~~- or maybe I could just return the labeled br instr and extract the label from there~~
    ~~2. push `index` onto the instr stack~~
    ~~3. push br_table with generated vector of indeces last~~

    ~~- ??? how to deal with bs and rs getting assigned the same value all over?~~
    ~~- ??? how to deal with index that jumps out of a whole function (we haven't parsed it yet)~~
      ~~- [V] just rewrite target logic such that wasm_label = block_stack.len() - target~~

    ~~// [ ] result_stack reset @Michelle's suggestion *are we resetting result_stack because it can be the same for different cases?*~~
    ~~// - how to deal with returns inserting drops for blockresults~~
    ~~// [ ] at the end of each case being translated, check the rhs, and if it's equal, remove the last instr *why only the last??*~~
      ~~// [ ] check only the top of vec of instr and add it before stack~~
        ~~// [ ] do this only when ALL the cases are the same~~


  ~~- ??? technically, `br_table_result` could be optimized such that putting the const on the stack is the only instruction made, right? *Yes*~~


~~- ??? what to do with drop production for cases like br_if_result? Bs are being reassigned~~
  ~~- [V] pass brresults **map** as an arg to dewimp stmt~~
    ~~- [V] replace blockresult_count logic with set processing~~
    ~~- [V] test that the result assigned != current result; if it is - skip~~
  ~~- ??? what is the purpose of reassignement?~~
  ~~- ??? why first `b1` is not assigned to `s0`? and why not second `b1` made `= s0` ?~~


~~- ??? How are stack variables produced by wimplify?`~~
  ~~- Short answer, stack variables are produced whenever there’s an “effect-ful” instruction, ie, the instructions in wimplify that have a call to materialize_as_statements (or a function with a similar name)~~
  ~~- ??? How stack vars work for `drop_order.wat` ? Why some consts are just expressions and some are stack vars?~~
  ~~- ??? How stack variable production affects drop insertion logic?~~

~~- ??? Do we assume wimpl to be handwritten? If we do, it might change the logic of how s0 are put on the stack and dropped. In particular, we’d have to add drops differently *We dont*~~

~~- ??? ❓ Show how drops are implemented and do a sanity check. Can there be a case that by dropping values at the very end we are dropping actual results vs unused values?~~

~~- [V] test everything again (since changed the wasm_target logic)~~

=== ** ===


~~- ??? is there a value in inserting asserts in each function like its done in wimplify? *not really, it's done there if wasm binary was modified manually; but no one modifies wimpl since its an IR*~~

- ??? gotta refactor a bunch, esp separate function parsing, in order to implement the function call

    // 1. Logic
    // 2. Naming
    // 3. Structure refactoring


    // TODO fn dewimplify module
      // module -> fn
      // [ ] how to process several functions within the module?
          // [ ] transpile every function in the module in a loop
      // why am I passing wasm_mod as an arg?

    // [ ] **use Metadata struct !!!**


    // ??? why wimpls look different? 
    // ??? why am I getting a nom Some(tag) parsing error?



    // TODO fn dewimplify function
      // [ ] what is there to add to a function specifically?
      //
      // TODO @Dmitrii translate and dewimplify function
      // a vector of initialized locals is added here


  - [ ] only call_evalution passes right now because of the parser
  - [ ] push args as instrs, then the called function
    - [ ] same for call indirect

- [ ] function call
  - [ ] test `local_updated` after call implemented
  - [ ] test `drop_later` when call implemented

- [ ] return all (potentially) deleted tests (wat to wasm)
  - [ ] restore drop_order.wasm
  - [ ] restore drop_single
  - [ ] restore drop_test
  - [ ] restore block_nested
  - [ ] block_result
  - [ ] block_single
  - [ ] br_nested_simple

- [ ] Create SAME TEST parametrized
  - [ ] Do NOT delete the old ones (make a copy folder)
    - [ ] blocks, drops, locals, brs first

  - [ ] test br_table with parameters as switch values https://musteresel.github.io/posts/2020/01/webassembly-text-br_table-example.html
  - [ ] test the second example there (without globals probably)


=== *waiting on parse.rs rewrire (after ~18.11)* ===


- [ ] Assign -> Global
- ❓ waiting on Michelle to implement the parser 
  - ??? are globs always `mut type` or can they be just `type`?
- [ ] Store globals as a separate vector (set) of values independent of instructions?? (see `Module::globals`)
    ??? why does wasm function.code.locals return Vec<Local, Global> ?

- [ ] fix br into continue for loops (if Michelle implements this)
- [ ] fix ifs 

- [ ] Implement func_type assignement from the parsed wimpl module

- [ ] refactor dewimpl test into module and function units

~~- does wasm `select` consume its operands?~~
~~    - it does~~

// TODO Impl Module
    // TODO parse "module {" string
        // TODO parse "function"

// syntax:
/*  module {
    func f (p0: i32) -> (r0: i32) {
        r0: i32 = i32.add(i32.const 3, p0)
        return r0
    }
}
*/

- [ ] Parse func export names from Wimpl

- [ ] refactor dewimpl test into a dewimpl_module and then a bunch of tests (as separate functions?)

- [ ] refactor wasmtime setup into a separate function

- [ ] see what else can be refactored

=== *makes sense to do this after the refactoring above* ===

- Can’t test Multipl Returs because multiple values are not supported by wimplify (only WASM MVP is supported)
  - if fixed:
    - [ ] fix br_table_test_3.wat
      - `br_table_test_3.wimpl` - there should be a drop before `b1`s value in .wasm (see .wat) but our blockresult Assign logic doesn't push drops for blockresults. **Ideas? Just ignore this?**
    - [ ] fix multiple returns logic
    - [ ] change frame result type into vector of types

- [ ] Set up dewimplify.rs and put all code there

- [ ] Set up a State of dewimplify parameters <- include all parameters there

- [ ] DOCUMENT THAT .tee is not supported
- [ ] DOCUMENT THAT local drop is not supported
- [ ] DOCUMENT THAT wasmtime invoke might break in the future and is an experimental feature


- add FUNCTION ANNOTATIONS (live documentation)

=== 

- ??? how to add a `wasmtime` shell utility as a dependency?
    - [ ] maybe install it as a cargo


- [ ] How do I wrap asserts such that they don’t panic?
- [ ] Fix “error” into something real
- [ ] Use expect vs unwrap

