
include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

include runtests.f

include benchmark.f

6 7 ' T9 C6 3 .s cr \ expecting 6 7 13 3

\ HEX
\ : rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
\ 1234 5678 $abc $def rp1 .s

.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr
