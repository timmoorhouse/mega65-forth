\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

CR .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) CR

include prelimtest.fth
include tester.fr
\ S" ttester.fs" INCLUDED
true verbose !

  s" core.fr"          included
  s" coreplustest.fth" included
  s" utilities.fth"    included
  s" errorreport.fth"  included
  s" coreexttest.fth"  included
  s" blocktest.fth"    included
  s" doubletest.fth"   included
  s" exceptiontest.ft" included   \ note file name truncation
  s" facilitytest.fth" included
  s" filetest.fth"     included
  s" localstest.fth"   included
  s" memorytest.fth"   included
  s" toolstest.fth"    included
  s" searchordertest." included   \ note file name truncation
  s" stringtest.fth"   included

include internalstest.f

REPORT-ERRORS

CR .( Forth tests completed ) CR CR
