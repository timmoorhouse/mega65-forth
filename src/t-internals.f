
environment-wordlist forth-wordlist internals-wordlist 3 set-order

\ gforth:
\ TODO savesystem

\ internals:

T{ 0 -> #0 }T

T{ 1 -> #1 }T

T{ 2 -> #2 }T

T{ #100 2+ -> #102 }T
T{ 0 2+ -> 2 }t
T{ #-100 2+ -> #-98 }T

T{ #100 2- -> #98 }T
T{ 0 2- -> -2 }t
T{ #-100 2- -> #-102 }T

\ TODO 0branch
\ TODO branch
\ TODO (cliteral)
\ TODO (literal)
\ TODO ?immediate
\ TODO latest
\ TODO latestxt
\ TODO lower
\ TODO out
\ TODO rp!

: rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
T{ 1234 5678 $abc $def rp1 -> 1234 5678 }T

\ TODO s>number?

T{ 1 2 3 sp@ @ -> 1 2 3 3 }T
T{ 1 2 3 sp@ cell+ @ -> 1 2 3 2 }T

T{ 1 2 3 sp@ 4 5 rot sp! -> 1 2 3 }T

\ TODO !csp
\ TODO ?csp
\ TODO ?exec
\ TODO ?pairs
\ TODO ?stack
\ TODO ?terminal
\ TODO ?error
\ TODO error

\ mega65
\ TODO background
\ TODO border
\ TODO foreground
\ TODO mon

only forth definitions

cr .( End of internals tests ) cr
