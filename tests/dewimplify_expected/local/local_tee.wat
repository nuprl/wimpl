(module
  (func $f0

    ;; FIXME @Dmitrii: had to change the name of the local from @l0 to no name since wimpl doesn't support named locals

    (local i32)

    ;; FIXME @Dmitrii change this test to get rid of drops (maybe split one test into 3???)
      ;; on the the other hand – local.tee doesn't work anyway

    ;; See the expected runtime stack and local state on the right.
    ;; i32.const 3   ;; [3]
    ;; local.tee 0 ;; [3], {$l0 -> 3}
    ;; i32.const 7   ;; [3, 7]
    ;; local.set 0 ;; [3], {$l0 -> 7}
    ;; local.get 0 ;; [3, 7]
    ;; i32.add       ;; [10]
    ;; drop          ;; []

    ;; The same exact code with local.set and local.get instead of local.tee
    i32.const 3   ;; [3]
    local.set 0 ;; [], {$l0 -> 3}
    local.get 0 ;; [3], {$l0 -> 3}
    i32.const 7   ;; [3, 7]
    local.set 0 ;; [3], {$l0 -> 7}
    local.get 0 ;; [3, 7]
    i32.add       ;; [10]
    drop          ;; []

    ;; This should translate to only a single assignment.
    ;; i32.const 3   ;; [3]
    ;; local.tee $l0 ;; [3], {$l0 -> 3}
    ;; local.get $l0 ;; [3, 3], {$l0 -> 3}
    ;; i32.add       ;; [6]
    ;; drop          ;; []
  )
  (export "test" (func $f0))
)
