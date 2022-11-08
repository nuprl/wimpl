(module
    (type $0 (func (param i32)))
    (func $1 (param i32)
        block (result i32)
            i32.const 42 		
            block 
            	i32.const 0 
            	drop
            end 
            local.get 0
            i32.add            
        end
        drop         
    )
    (export "test" (func $1))
)
