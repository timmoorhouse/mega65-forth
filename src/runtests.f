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
\ S" exceptiontest.fth" INCLUDED
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

CR .( Forth tests completed ) CR CR


