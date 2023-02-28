
\ active exception handler
variable handler
0 handler !

\ -1    ABORT
\ -2    ABORT"
\ -3    stack overflow
\ -4    stack underflow
\ -5    return stack overflow
\ -6    return stack underflow
\ -7    do loops nested too deeply during execution
\ -8    dictionary overflow
\ -9    invalid memory address
\ -10   division by zero
\ -11   result out of range
\ -12   argument type mismatch
\ -13   undefined word
\ -14   interpreting a compile-only word
\ -15   invalid FORGET
\ -16   attempt to use zero length string as a name
\ -17   pictured numeric output string overflow
\ ...



\ TODO CATCH
\ : catch     ( xt -- exception# | 0 )
\     sp@ >r              ( xt )       \ save data stack pointer
\     handler @ >r        ( xt )       \ and previous handler
\     rp@ handler !       ( xt )       \ set current handler
\     execute             ( )          \ execute returns if no THROW
\     r> handler !        ( )          \ restore previous handler
\     r> drop             ( )          \ discard saved stack ptr
\     0 ;                 ( 0 )        \ normal completion

\ TODO THROW
\ : throw     ( ??? exception# -- ??? exception# )
\     ?dup if             ( exc# )     \ 0 THROW is no-op
\         handler @ rp!   ( exc# )     \ restore prev return stack
\         r> handler !    ( exc# )     \ restore prev handler
\         r> swap >r      ( saved-sp ) \ exc# on return stack
\         sp! drop r>     ( exc# )     \ restore stack
\         \ Return to the caller of CATCH because return
\         \ stack is restored to the state that existed
\         \ when CATCH began execution
\     then ;

.( ... end of exception.f ) cr
