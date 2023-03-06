
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

s" bootstrap-full.f" included

:noname
  case
  3 of  4 foreground endof \ error  - purple
  2 of 14 foreground endof \ prompt - lt blue
  1 of  7 foreground endof \ input  - yellow
  ( 0 ) 1 foreground       \ output - white
  endcase ; is theme

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

unused . s" bytes free" type cr \ 26594 first, then 26460 after reload? getting fib, benchmark ?!?!?!
\ HERE needs to get saved!

\ include runtests.f

include benchmark.f

1 2 3   asdjfklj   4 5 6

\ HEX
\ : rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
\ 1234 5678 $abc $def rp1 .s

.( Completed bootstrap ) cr
