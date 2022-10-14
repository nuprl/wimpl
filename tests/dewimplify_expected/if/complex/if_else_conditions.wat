(;
Hint: For an IF statement that returns (puts on a stack) a value from its `then` or `else` blocks => we _always_ have to define the type of the return (`result`) value at the beginning of the IF block
- if the type of the result value is defined => the compiler will check that the `then` and `else` blocks return the same type of value; in other words, they _must_ return the same type of value
;)

(module
    (func $0
		(; a test with a complex condition ;)
		i32.const 1
		i32.const 0 
		i32.add
		i32.const 2
		i32.eq
		(if (result i32)
		  (then
		    i32.const 1
		  )	
		  (;a `then` block _always_ precedes an `else` block;)
		  (else
			i32.const 2
		  )	  
		)
		i32.const 2
		i32.eq
		drop
    )
)