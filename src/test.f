\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

cr .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) cr

include t-preliminary
include t-tester
\ true verbose !

  s" t-core"         included
  s" t-core-plus"    included
  s" t-utilities"    included
  s" t-error-report" included
  s" t-core-ext"     included
  s" t-block"        included
  s" t-double"       included
  s" t-exception"    included
  s" t-facility"     included
\ TODO floating
  s" t-file"         included
  s" t-locals"       included
  s" t-memory"       included
  s" t-tools"        included
  s" t-search"       included
  s" t-string"       included
\ TODO xchar

include t-internals

report-errors

cr .( Forth tests completed ) cr cr
