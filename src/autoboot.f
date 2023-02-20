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
\ include prelimtest.fth

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

\ 1234 CONSTANT CTEST
\ CTEST 1234 = .s cr
.( line 1 ) cr
here . cr
: foo2 ." foo bar baz " ;
.( line 2 ) cr
foo2
.( line 3 ) cr
.s cr 

\ TODO delete autoboot.f? rename it?
