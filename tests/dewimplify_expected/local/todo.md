// FIXME delete this file

// 🔥 Test each feature here

// [V] gotta compare locals now in my tests (not just instructions)
// [V] set up module => functions[0] => code => locals and functions[0] => code => body from wimpl module => functions[0] => body


// [V] count the number of params                    
    // - collect a params set in a *** separate pass ***
        // - then pass the set to dewimpl statements fnc

// [V] pass locals as a param to dewimpl statements
// [V] pass returns as a param to dewimpl statements

// [V] process assigns

        // [V] if lhs == local && local is not in locals
            // vector of locals has to be set IN THE ORDER IT WAS DECLARED
            // - add it to locals
                // - get __only the type__
                    // from Assign parameters
        // [V] else
            // parse the rhs: 
                // VarRef => local.get (using the exact numbering for params and params+offset for locals) 
                    // if VarRef to Stack => do nothing
            // parse the lhs => local.set (using the exact numbering for params and params+offset for locals)

        // [V] stack locals => just translate them as their corresponding rhs?
            // !! but that will lead to local.set+local.get translation instead of local.teeS 
                // **do this as a first iteration**
                // ??? consider peek() and consume()

        // [V] returns - just parse the instrs?

    
    // [V] store locals in function->code->locals

// [V] use locals and returns to generate type_
    // [V] rewrite params to store types
    // [V] set up returns
    // [V] gotta compare _type now in my tests (not just instructions)
    // [V] set up wasm module => functions[0] => type_ from wimpl module => functions[0] => type_

// TODO globals - same as locals?
    // ??? why does wasm function.code.locals return Vec<Local, Global> ?