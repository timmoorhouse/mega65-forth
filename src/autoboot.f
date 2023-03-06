
include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

include runtests.f

:noname
  case
  3 of  4 foreground endof \ error  - purple
  2 of 14 foreground endof \ prompt - lt blue
  1 of  7 foreground endof \ input  - yellow
  ( 0 ) 1 foreground       \ output - white
  endcase ; is theme


include benchmark.f

1 2 3   asdjfklj   4 5 6

\ HEX
\ : rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
\ 1234 5678 $abc $def rp1 .s

.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr
