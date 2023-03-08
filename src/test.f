\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

cr .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) cr

include t-preliminary.f
include t-tester.f
\ true verbose !

  s" t-core.f"         included
  s" t-core-plus.f"    included
  s" t-utilities.f"    included
  s" t-error-report.f" included
  s" t-core-ext.f"     included
  s" t-block.f"        included
  s" t-double.f"       included
  s" t-exception.f"    included
  s" t-facility.f"     included
\ TODO floating
  s" t-file.f"         included
  s" t-locals.f"       included
  s" t-memory.f"       included
  s" t-tools.f"        included
  s" t-search.f"       included
  s" t-string.f"       included
\ TODO xchar

include t-internals.f

report-errors

cr .( Forth tests completed ) cr cr
