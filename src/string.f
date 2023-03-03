
\ The following words are implemented internally:

\ CMOVE CMOVE> COMPARE

: blank ( c-addr u ) bl fill ;

\ TODO sliteral
\ TODO this only works for strings of length <= 255
\ : sliteral ( c-addr u -- ) ( -- c-addr u ) postpone (c") dup c, swap over here swap cmove allot ; immediate compile-only

.( ... end of string.f ) cr