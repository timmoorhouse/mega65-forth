
include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

include runtests.f

.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr
