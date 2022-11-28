(module
    (func $1 (result i64)
		(block (result i64)
            i32.const 1
			(if (result i64)
		  	(then
		    	i64.const 1
		  	)	
		  	(else
				i64.const 2
		  	))
			(i32.const 2)  ;; value used to select a branch
			(br_table 1 0 1) ;; last is default  ;; @Dmitrii 1 0 1 are branch targets
		)
	)
	(export "test" (func $1))
)
