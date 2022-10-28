// FIXME @Dmitrii delete this file

=== Week 17.10 ===

~~// [V] gotta compare locals now in my tests (not just instructions)~~
~~// [V] set up module => functions[0] => code => locals and functions~~~~[V] => code => body from wimpl module => functions[0] => body~~

~~// [V] count the number of params                    ~~
~~    // - collect a params set in a *** separate pass ***~~
~~        // - then pass the set to dewimpl statements fnc~~

~~// [V] pass locals as a param to dewimpl statements~~
~~// [V] pass returns as a param to dewimpl statements~~

~~// [V] process assigns~~

~~        // [V] if lhs == local && local is not in locals~~
~~            // vector of locals has to be set IN THE ORDER IT WAS DECLARED~~
~~            // - add it to locals~~
~~                // - get __only the type__~~
~~                    // from Assign parameters~~
~~        // [V] else~~
~~            // parse the rhs: ~~
~~                // VarRef => local.get (using the exact numbering for params and params+offset fo~~r locals) 
~~                    // if VarRef to Stack => do nothing~~
~~            // parse the lhs => local.set (using the exact numbering for params and params+offset~~ for locals)

~~        // [V] stack locals => just translate them as their corresponding rhs?~~
~~            // !! but that will lead to local.set+local.get translation instead of local.tee~~
~~                // **do this as a first iteration**~~
~~                // consider peek() and consume()~~

~~        // [V] returns - just parse the instrs?~~

~~    // [V] store locals in function->code->locals~~

~~// [V] use locals and returns to generate type_~~
~~    // [V] rewrite params to store types~~
~~    // [V] set up returns~~
~~    // [V] gotta compare _type now in my tests (not just instructions)~~
~~    // [V] set up wasm module => functions[0] => type_ from wimpl module => functions[0] => type_~~

=== Week 24.10 ===

**🔥 Test features bit by bit**

- [V] wasmtime for one function
  - ??? how to add a shell utility as a dependency?

- [X] globals *not implemented in parsing*

- [X] Stack tests *(Went through all the wimplify test that use stack vars and also through some of tests.rs, but haven't found anything _immediate_ to test)*
  - ??? I see how there might appear problems with the stack when we introduce control structures and types such as memorysize, but I don't see how this is a problem with the stack when we're just assigning a value to a variable.
  - ??? Unclear how stack references as operands work
    - [ ] find if there are test cases where the stack var refers to a value that is already OFF wasm stack (e.g.Test 2 below) and check the corresponding .wat file for whether the value is being put back on the wasm stack


- [ ] Loops
- [ ] Br
- [ ] BrIF
- [ ] BrTbl

===

- [ ] Assign -> Global
- ❓ waiting on Michelle to implement the parser 
  - ??? are globs always `mut type` or can they be just `type`?
- [ ] Store globals as a separate vector (set) of values independent of instructions?? (see `Module::globals`)
    ??? why does wasm function.code.locals return Vec<Local, Global> ?

- [ ] Implement func_type assignement from the parsed wimpl module

===

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

- [ ] refactor wasmtime setup into a separate function

- [ ] see what else can be refactored

===

- [ ] Set up dewimplify.rs and put all code there

- [ ] Set up a State of dewimplify parameters <- include all parameters there

- [ ] DOCUMENT THAT .tee is not supported
- [ ] DOCUMENT THAT local drop is not supported

- [ ] How do I wrap asserts such that they don’t panic?
- [ ] Fix “error” into something real
- [ ] Use expect vs unwrap

