(module
  (type (;0;) (func (param ) (result i32)))
  (func $1 (type 0) (param ) (result i32)

    ;; Test 1 = PASS Testing for a stack variable evaluating to a constant
    i32.const 20
    ;; i32.const 10 // FIXME @Dmitrii multi value results are not supported yet (only wasm MVP is supported)

    ;; Test 2 = FAIL because s0 is off the stack already and there is only one parameter for i32.add
    ;; i32.const 1
    ;; i32.const 1
    ;; i32.eq
    ;; i32.add
    ;; drop

  )
  (export "test" (func $1))  
)


