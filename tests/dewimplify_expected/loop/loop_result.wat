;; this loop doesn't loop
(module
  (type (;0;) (func (param i32) (result i32)))
  (func $1 (type 0) (param i32) (result i32)
    (loop (result i32)
      local.get 0   	
      i32.const 1
      i32.add
	  )
  )
  (export "test" (func $1))
)


