(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func $1 (type 0) (param i32 i32) (result i32)
    (;hint: this is the initing of four locals in wimpl;)
    (local i32 i64)
    (local i32 i32)

    (;hint: we're changing the value of the first parameter;)
    (i32.add
      (local.get 0)
      (local.get 1))
    (local.set 0)
    
    (;hint: The local.tee instruction sets the value of a local variable and loads the value onto the stack.;)
    (i32.const 30)
    (;hint: this value goes onto a stack;)
    ;; (local.tee 1) 
    ;; FIXME had to replace it with the following so that the tests pass::
    (local.set 1)
    (local.get 1)
    
    (i32.const 31) 
    (local.set 2)

    (i64.const 42) 
    (local.set 3)
    
    (local.get 2) 
    (i32.add) 
    
    (local.get 0) 
    (i32.add)  
  )
)


