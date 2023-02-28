\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

CR .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) CR

include prelimtest.fth
include tester.fr
\ S" ttester.fs" INCLUDED
\ true verbose !

include core.fr
include coreplustest.fth
include utilities.fth
include errorreport.fth
include coreexttest.fth
\ S" blocktest.fth" INCLUDED
\ include doubletest.fth
\ include exceptiontest.fth
\ S" facilitytest.fth" INCLUDED
\ S" filetest.fth" INCLUDED
\ S" localstest.fth" INCLUDED
\ S" memorytest.fth" INCLUDED
\ S" toolstest.fth" INCLUDED
\ S" searchordertest.fth" INCLUDED
\ S" stringtest.fth" INCLUDED

\ TODO fix s" so these work ...

\ S" prelimtest.fth" INCLUDED
\ S" tester.fr" INCLUDED
\ \ S" ttester.fs" INCLUDED
\ 
\ S" core.fr" INCLUDED
\ S" coreplustest.fth" INCLUDED
\ S" utilities.fth" INCLUDED
\ \ S" errorreport.fth" INCLUDED      \ TODO needs does>
\ S" coreexttest.fth" INCLUDED
\ \ S" blocktest.fth" INCLUDED
\ S" doubletest.fth" INCLUDED
\ \ S" exceptiontest.fth" INCLUDED
\ \ S" facilitytest.fth" INCLUDED
\ \ S" filetest.fth" INCLUDED
\ \ S" localstest.fth" INCLUDED
\ \ S" memorytest.fth" INCLUDED
\ \ S" toolstest.fth" INCLUDED
\ \ S" searchordertest.fth" INCLUDED
\ \ S" stringtest.fth" INCLUDED
\ \ REPORT-ERRORS

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
\ TODO ?hidden
\ TODO ?immediate
\ TODO latest
\ TODO latestxt
\ TODO lower
\ TODO out
\ TODO rp!

: rp1 >r >r >r >r rp@ @ rp@ cell+ @ 2r> 2r> 2drop 2drop ;
T{ 1234 5678 $abc $def rp1 -> 1234 5678 }T

\ TODO s>number?

T{ 1 2 3 sp@ @ -> 1 2 3 3 }T
T{ 1 2 3 sp@ cell+ @ -> 1 2 3 2 }T

T{ 1 2 3 sp@ 4 5 rot sp! -> 1 2 3 }T

\ TODO !csp
\ TODO ?csp
\ TODO ?comp
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

CR .( Forth tests completed ) CR CR
