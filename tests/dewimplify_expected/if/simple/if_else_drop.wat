(;
hint @Dmitrii : an if or else block that drops the value is going to be parsed into wimpl such that both if and else bodies are _empty_ !
;)

(module
    (func $0
        i32.const 0 
		(if
		  (then
		  )
		  (else
		  )
		)                 
    )
  (export "test" (func $0))
)
