.( Starting bootstrap... ) cr

include bootstrap-min.f

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr

\ TODO redirect output
include prelimtest.fth

\ TODO delete autoboot.f? rename it?

