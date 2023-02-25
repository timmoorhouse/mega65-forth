
include bootstrap-min.f

\ Note that you can't use comments until after including bootstrap-min!

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

include bootstrap-full.f

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

\ TODO redirect output
include prelimtest.fth          \ 1 failure
include tester.fr               \ OK
\  \ include ttester.fs
include core.fr                 \ LOTS to fix
include coreplustest.fth        \
\ include utilities.fth
\ include errorreport.fth
include coreexttest.fth         \
\ include blocktest.fth
\ include doubletest.fth
\ include exceptiontest.fth
\ include facilitytest.fth
\ include filetest.fth
\ include localstest.fth
\ include memorytest.fth
\ include toolstest.fth
\ include searchordertest.fth
\ include stringtest.fth
\ report-errors
cr .( Forth tests completed ) cr cr

\ : Foo2 ( -- u) 5 3 if 7 else 9 then 1+ ;
\ : foo2 ( -- ) 5 4 begin .s again ;
\ : foo2 0 begin .s 1+ dup 10 > until drop ;
\ : foo2 0 begin  1+ dup 10 < while .s repeat drop ;
\ : foo2 ( -- ) 7 3 do i . space loop ;
\ : foo2 ( -- ) 12 3 do i . space 2 +loop ;
\ : foo2 ( -- 12 ) [ 12 ] literal ;
\ : foo2 ( -- 40 ) [char] (foo) ;
\ : foo2 #99 $ab -3 'a' ;
\ : foo2  ( -- n )  0 10 0 DO DUP 5 = IF LEAVE ELSE 1+ THEN LOOP ;
\ : foo2  ( -- n )  0 10 0 DO leave LOOP ;
\ : foo2  ( -- ch )  [char] 0 [char] a [CHAR] A ;
\ foo2 cr .s cr

.( defer foo ) cr
defer foo 
.( ' foo ... ) cr
' foo . cr
.( : foo2 ... ) cr
: foo2 ." in foo2 " $1234 ;
.( ' foo2 ) cr
' foo2 . cr
.( ' foo2 execute ) cr
' foo2 execute .s cr
.( foo2 is foo ) cr
' foo2 is foo
.( foo contains xt ... ) cr
' foo >body @ . cr
.( foo ) cr
foo
.s cr


.( Completed bootstrap ) cr
.( TODO autoboot.f should now be deleted or renamed ) cr
