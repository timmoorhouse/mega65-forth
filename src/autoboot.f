
include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

include runtests.f

include benchmark.f

\ : rp1 >r >r >r >r rp@ @ rp@ cell+ @ 2r> 2r> 2drop 2drop ;
\ hex
\ 1234 5678 1bcd 1357
\ rp1 .s

.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr
