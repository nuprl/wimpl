(module
   (func $1 (result i32) 
        block (result i32) (; @Dmitrii returns 42 ;)
            i32.const 42	
            block 			
				i32.const 0 (; @Dmitrii this is dropped implicitly ;)
				br 1 
			end 
        end					
    )
    (export "test" (func $1))
)
