
\ The following words are implemented internally:

\ CMOVE CMOVE> COMPARE

\ ***************************************************************************

: /string ( c-addr1 u1 n -- c-addr2 u2 ) dup >r - swap r> + swap ;

: blank ( c-addr u ) bl fill ;

\ SLITERAL see core.f

.( ... end of string.f ) cr