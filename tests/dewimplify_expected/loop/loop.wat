(module
  (func
    ;; create a local variable (by default initialized to 0)
    (local i32)
    (loop
      ;; add one to local 0
      local.get 0
      i32.const 1
      i32.add
      local.set 0
      ;; if local 0 is less than 10 branch to loop
      local.get 0
      i32.const 10
      i32.lt_s
      br_if 0
    )
  )
)
