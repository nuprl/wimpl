(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func $1 (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    (local i32 i32)

    (i32.add
      (local.get 0)
      (local.get 1))
    (local.set 0)
    
    (i32.const 30)
    (local.tee 1) 
    
    (i32.const 31) 
    (local.set 2)

    (i32.const 42) 
    (local.set 3)
    
    (local.get 2) 
    (i32.add) 
    
    (local.get 0) 
    (i32.add)  
  )
)


