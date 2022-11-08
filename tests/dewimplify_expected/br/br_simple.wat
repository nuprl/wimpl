(module
    (type (;0;) (func (param) (result i32)))
    (func $1 (;0;) (type 0) (result i32)
        block (result i32)
            i32.const 42
			br 0 
        end    
    )
    (export "test" (func $1))
)
