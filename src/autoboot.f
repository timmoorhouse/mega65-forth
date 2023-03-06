
\ include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

:noname ; is autoboot


: savesystem ( "<spaces>name" -- ) parse-name w/o open-file drop \ TODO check status from open-file
  >r ( R: fid )
  sp@ 2 r@ write-file drop \ TODO check status from write-file
  $2001 dup here swap - r@ write-file drop \ TODO check status
  r> close-file drop \ TODO check status
  ;

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

  s" block.f"         included
  s" block-ext.f"     included
\ s" core.f"          included    \ Embedded
\ s" core-ext.f"      included    \ Embedded
  s" double.f"        included
  s" double-ext.f"    included
  s" exception.f"     included
\ s" exception-ext.f" included    \ TODO no need for one yet
  s" facility.f"      included
  s" facility-ext.f"  included
\ s" file.f"          included    \ Embedded
  s" file-ext.f"      included
  s" floating.f"      included
  s" floating-ext.f"  included
  s" locals.f"        included
  s" locals-ext.f"    included
  s" memory.f"        included
\ s" memory-ext.f"    included    \ TODO no need for one yet
  s" search.f"        included
  s" search-ext.f"    included
  s" string.f"        included
  s" string-ext.f"    included
  s" tools.f"         included
  s" tools-ext.f"     included
  s" xchar.f"         included
  s" xchar-ext.f"     included

:noname
  case
  3 of  4 foreground endof \ error  - purple
  2 of 14 foreground endof \ prompt - lt blue
  1 of  7 foreground endof \ input  - yellow
  ( 0 ) 1 foreground       \ output - white
  endcase ; is theme

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

unused . s" bytes free" type cr \ 26552

\ include runtests.f

include benchmark.f

1 2 3   asdjfklj   4 5 6

\ HEX
\ : rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
\ 1234 5678 $abc $def rp1 .s

.( Completed bootstrap ) cr
