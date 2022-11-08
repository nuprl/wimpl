(module
  (type $0 (func (param i32 i32) (result i32)))
  (func $0 (param i32 i32) (result i32)
    block (result i32)
        i32.const 4 (;; b1: i32 = i32.const 4 ;;)
    end (;; s0: i32 = b1 ;;)
    block (result i32)
        i32.const 5 (;; b2: i32 = i32.const 5 ;;)
    end (;; s1: i32 = b2 ;;)
    local.get 0
    local.get 1
    drop
    drop
    i32.add
  )
  (export "test" (func $0))
)
