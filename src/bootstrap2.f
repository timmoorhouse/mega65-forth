
  s" d-core.f"          included
  s" d-core-ext.f"      included
  s" d-file.f"          included

:noname ; is autoboot \ TODO could use decimal to save a few bytes

: savesystem ( "<spaces>name" -- ) parse-name w/o open-file drop \ TODO check status from open-file
  >r ( R: fid )
  sp@ 2 r@ write-file drop \ TODO check status from write-file
  $2001 dup here swap - r@ write-file drop \ TODO check status
  r> close-file drop \ TODO check status
  ;
  
:noname ( -- ) postpone [ 0 to source-id
  begin
    refill drop \ TODO check result from REFILL
    (evaluate)
    state @ 0= if 
      2 theme \ prompt
    then 
  again ; is (quit)

\ Before saving a system, the following deferred words MUST be defined:
\ . .S AUTOBOOT (QUIT)

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

  s" d-block.f"         included
  s" d-block-ext.f"     included
  s" d-double.f"        included
  s" d-double-ext.f"    included
  s" d-exception.f"     included
\ s" d-exception-ext.f" included    \ TODO no need for one yet
  s" d-facility.f"      included
  s" d-facility-ext.f"  included
  s" d-file-ext.f"      included
  s" d-floating.f"      included
  s" d-floating-ext.f"  included
  s" d-locals.f"        included
  s" d-locals-ext.f"    included
  s" d-memory.f"        included
\ s" d-memory-ext.f"    included    \ TODO no need for one yet
  s" d-search.f"        included
  s" d-search-ext.f"    included
  s" d-string.f"        included
  s" d-string-ext.f"    included
  s" d-tools.f"         included
  s" d-tools-ext.f"     included
  s" d-xchar.f"         included
  s" d-xchar-ext.f"     included

:noname
  case
  3 of  4 foreground           endof \ error  - purple
  2 of 14 foreground ." ok" cr endof \ prompt - lt blue
  1 of  7 foreground           endof \ input  - yellow
  ( 0 ) 1 foreground                 \ output - white
  endcase ; is theme

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

.( ... bootstrap stage 2 complete ) cr cr

unused . s" bytes free" type cr \ 26693

: test s" test.f" included ;

: bm s" benchmark.f" included ;

1 2 3   asdjfklj   4 5 6

.( end of bootstrap2.f ) cr
