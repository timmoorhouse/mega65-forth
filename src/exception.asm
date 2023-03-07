
; ****************************************************************************
; EXCEPTION

HANDLER
        !word 0

        +WORD ".e", 0
W_DOTE
        !word DO_DEFER
        !word W_SIMPLE_DOTE

; TODO move this to bootstrap? would require adding exception to the minimal config
        +NONAME
W_SIMPLE_DOTE        ; (n --)
        !word DO_COLON
        +DOTQ "exception "
        !word W_DOT
        !word W_CR
        !word W_PSEMI

        +WORD "save-location", 0
W_SAVE_LOCATION
        !word DO_DEFER
        !word W_NOOP

        +WORD "clear-location", 0
W_CLEAR_LOCATION
        !word DO_DEFER
        !word W_NOOP

; ****************************************************************************
; CATCH
; ANSI 9.6.1.0875

!if ENABLE_EXCEPTION {
        +WORD "catch", 0
} else {
        +NONAME
}
W_CATCH
        !word DO_COLON
        !word W_CLEAR_LOCATION
        !word W_SPAT
        !word W_TOR
        +LITERAL HANDLER
        !word W_AT
        !word W_TOR
        !word W_RPAT
        +LITERAL HANDLER
        !word W_STORE
        !word W_EXECUTE
        !word W_RFROM
        +LITERAL HANDLER
        !word W_STORE
        !word W_RFROM
        !word W_DROP
        !word W_ZERO
        !word W_PSEMI

; : catch     ( xt -- exception# | 0 )
;     sp@ >r              ( xt )       \ save data stack pointer
;     handler @ >r        ( xt )       \ and previous handler
;     rp@ handler !       ( xt )       \ set current handler
;     execute             ( )          \ execute returns if no THROW
;     r> handler !        ( )          \ restore previous handler
;     r> drop             ( )          \ discard saved stack ptr
;     0 ;                 ( 0 )        \ normal completion

; ****************************************************************************
; THROW
; ANSI 9.6.1.2275

; TODO need to be able to throw from assembler

!if ENABLE_EXCEPTION {
        +WORD "throw", 0
} else {
        +NONAME
}
W_THROW
        !word DO_COLON
        !word W_QDUP
        +ZBRANCH +
        !word W_SAVE_LOCATION
        +LITERAL HANDLER
        !word W_AT
        !word W_RPSTORE
        !word W_RFROM
        +LITERAL HANDLER
        !word W_STORE
        !word W_RFROM
        !word W_SWAP
        !word W_TOR
        !word W_SPSTORE
        !word W_DROP
        !word W_RFROM
+        
        !word W_PSEMI

; : throw     ( ??? exception# -- ??? exception# )
;     ?dup if             ( exc# )     \ 0 THROW is no-op
;         handler @ rp!   ( exc# )     \ restore prev return stack
;         r> handler !    ( exc# )     \ restore prev handler
;         r> swap >r      ( saved-sp ) \ exc# on return stack
;         sp! drop r>     ( exc# )     \ restore stack
;         \ Return to the caller of CATCH because return
;         \ stack is restored to the state that existed
;         \ when CATCH began execution
;     then ;
