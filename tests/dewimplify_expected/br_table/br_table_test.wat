(module
    (func $1 (result i64)
		(block (result i64)
			(i64.const 42) ;; result value
			(i32.const 2)  ;; value used to select a branch
			(br_table 1 0 1) ;; last is default  ;; @Dmitrii 1 0 1 are branch targets
		)
        drop
        i64.const 0
	)
	(export "test" (func $1))
)
