
\ active exception handler
\ variable handler
\ 0 handler !

variable exception-line
variable exception-in
variable exception-input-buffer
variable exception-input-len

:noname ( -- )
  exception-input-buffer @ 0= if
    source-line @ exception-line !
    >in @ exception-in !
    source exception-input-len ! exception-input-buffer !
  then
  ; is save-location

:noname ( -- )
  0 exception-line !
  0 exception-in !
  0 exception-input-buffer !
  0 exception-input-len !
  ; is clear-location

: exception-message ( n -- c-addr u )
    case
    -1  of s" ABORT"                                         endof
    -2  of s" ABORTq"                                        endof
    -3  of s" stack overflow"                                endof
    -4  of s" stack underflow"                               endof
    -5  of s" return stack overflow"                         endof
    -6  of s" return stack underflow"                        endof
    -7  of s" do loops nested too deeply during execution"   endof
    -8  of s" dictionary overflow"                           endof
    -9  of s" invalid memory address"                        endof
    -10 of s" division by zero"                              endof
    -11 of s" result out of range"                           endof
    -12 of s" argument type mismatch"                        endof
    -13 of s" undefined word"                                endof
    -14 of s" interpreting a compile-only word"              endof
    -15 of s" invalid FORGET"                                endof
    -16 of s" attempt to use zero length string as a name"   endof
    -17 of s" pictured numeric output string overflow"       endof
    -18 of s" parsed string overflow"                        endof
    -19 of s" definition name too long"                      endof
    -20 of s" write to a read-only location"                 endof
    -21 of s" unsupported operation"                         endof
    -22 of s" control structure mismatch"                    endof
    -23 of s" address alignment exception"                   endof
    -24 of s" invalid numeric argument"                      endof
    -25 of s" return stack imbalance"                        endof
    -26 of s" loop parameters unavailable"                   endof
    -27 of s" invalid recursion"                             endof
    -28 of s" user interrupt"                                endof
    -29 of s" compiler nesting"                              endof
    -30 of s" obsolescent feature"                           endof     
    -31 of s" >BODY used on non-CREATEd definition"          endof
    -32 of s" invalid name argument (eg TO name)"            endof
    -33 of s" block read exception"                          endof
    -34 of s" block write exception"                         endof
    -35 of s" invalid block number"                          endof
    -36 of s" invalid file position"                         endof
    -37 of s" file I/O exception"                            endof
    -38 of s" non-existent file"                             endof
    -39 of s" unexpected end of file"                        endof
    -40 of s" invalid BASE for floating point conversion"    endof
    -41 of s" loss of precision"                             endof
    -42 of s" floating point divide by zero"                 endof
    -43 of s" floating point result out of range"            endof
    -44 of s" floating point stack overflow"                 endof
    -45 of s" floating point stack underflow"                endof
    -46 of s" floating point invalid argument"               endof
    -47 of s" compilation word list deleted"                 endof
    -48 of s" invalid POSTPONE"                              endof
    -49 of s" search order overflow"                         endof
    -50 of s" search order underflow"                        endof
    -51 of s" compilation word list changed"                 endof
    -52 of s" control flow stack overflow"                   endof
    -53 of s" exception stack overflow"                      endof
    -54 of s" floating point underflow"                      endof
    -55 of s" floating point unidentified fault"             endof
    -56 of s" QUIT"                                          endof
    -57 of s" exception in sending or receiving a character" endof
    -58 of s" [IF], [ELSE] or [THEN] exception"              endof
    -59 of s" ALLOCATE"                                      endof
    -60 of s" FREE"                                          endof
    -61 of s" RESIZE"                                        endof
    -62 of s" CLOSE-FILE"                                    endof
    -63 of s" CREATE-FILE"                                   endof
    -64 of s" DELETE-FILE"                                   endof
    -65 of s" FILE-POSITION"                                 endof
    -66 of s" FILE-SIZE"                                     endof
    -67 of s" FILE-STATUS"                                   endof
    -68 of s" FLUSH-FILE"                                    endof
    -69 of s" OPEN-FILE"                                     endof
    -70 of s" READ-FILE"                                     endof
    -71 of s" READ-LINE"                                     endof
    -72 of s" RENAME-FILE"                                   endof
    -73 of s" REPOSITION-FILE"                               endof
    -74 of s" RESIZE-FILE"                                   endof
    -75 of s" WRITE-FILE"                                    endof
    -76 of s" WRITE-LINE"                                    endof
    -77 of s" malformed xchar"                               endof
    -78 of s" SUBSTITUTE"                                    endof
    -79 of s" REPLACES"                                      endof
        >r 0 0 r>
    endcase ;

:noname ( n -- )
  cr
  2 theme \ prompt
  exception-line @ ?dup if ." line " . ." : " then
  3 theme \ error
  dup exception-message ?dup if type drop else drop ." exception " . then cr
  0 theme \ output
  exception-input-buffer @ exception-input-len @ type cr
  3 theme \ error
  exception-in @ 2- spaces '^' emit cr
  0 theme \ output
  ; is report-exception

\ TODO we should move CATCH to assembler so we can use it in ABORT

\ TODO CATCH
\ : catch     ( xt -- exception# | 0 )
\     sp@ >r              ( xt )       \ save data stack pointer
\     handler @ >r        ( xt )       \ and previous handler
\     rp@ handler !       ( xt )       \ set current handler
\     execute             ( )          \ execute returns if no THROW
\     r> handler !        ( )          \ restore previous handler
\     r> drop             ( )          \ discard saved stack ptr
\     0 ;                 ( 0 )        \ normal completion

\ TODO we should move THROW to assembler for use everywhere

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
