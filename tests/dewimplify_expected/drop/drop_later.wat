;; FIXME @Dmitrii test when func call implemented
(module
  (func $0
    i32.const 3
    i32.const 7
    call $f1 ;; removes 7, 3 still on stack
    drop ;; drops 3
  )
  (func $f1 (param i32)
    nop
  )
  (export "test" (func $0))
)
