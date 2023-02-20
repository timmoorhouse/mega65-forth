
\ ***************************************************************************
\ basic control flow building blocks

\ FIG control flow tags
\ 1 begin...[again|until]
\ 2 if...
\ 3 do... [loop|+loop]

\ Marks the origin of an unconditional forward branch
: ahead ( ?comp ) postpone branch here 0 , ( 2 ) ; immediate

\ Marks the origin of a conditional forward branch
: if ( ?comp ) postpone 0branch here 0 , ( 2 ) ; immediate

\ TODO use CS-PICK/CS-ROLL
\ Resolves an IF or AHEAD
: then ( ?comp 2 ?pairs ) here over - swap ! ; immediate

\ Marks the destination of a backwards branch
: begin ( ?comp ) here ( 1 ) ; immediate

: back ( ?comp ) here - , ; ( Resolve backward branch )

\ Resolves a BEGIN with an unconditional backwards branch
: again ( ?comp 1 ?pairs ) postpone branch back ; immediate

\ Resolves a BEGIN with a conditional backwards branch
: until ( ?comp 1 ?pairs ) postpone 0branch back ; immediate

\ ***************************************************************************
\ ... if [ ... else ] ... then

\ TODO change swap to 1 cs-roll ?

: else ( ?comp 2 ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate

: while postpone if 1 cs-roll ; immediate

: repeat postpone again postpone then ; immediate

\ TODO do ... loop is wrong
\ need to find where to exit to in a leave
\ - have address of end of loop inline after the (do)
\ - (do) copies end address to return stack
\ - (loop), (+loop) pulls from return stack on exit
\ - leave pulls from return stack and sets I

: do postpone (do) 0 , here ( 3 ) ; immediate

: loop ( 3 ?pairs ) postpone (loop) dup 2 - here 2 + swap ! back ; immediate

: +loop ( 3 ?pairs ) postpone (+loop) dup 2 - here 2 + swap ! back ; immediate

\ : WHILE   [COMPILE]  IF  2+  ;    IMMEDIATE
\ : REPEAT   >R  >R  [COMPILE]  AGAIN  R>  R>  2  -  [COMPILE]  ENDIF  ;  IMMEDIATE
       
\ ***************************************************************************

: >body ( xt -- a-addr ) 2+ ;

\ : abs ( n -- u ) dup 0< if negate then ;

: c, here c! 1 allot ;

: cell+ ( a-addr_1 -- a-addr_2 ) 2+ ;

: cells ( n_1 -- n_2 ) 2* ;

: char+ ( c-addr_1 -- c-addr_2 ) 1+ ;

: chars ( n_1 -- n_2 ) ;

: literal postpone (literal) , ; immediate

: [char] parse-name drop c@ postpone literal ; immediate

\ TODO alignment after string?
: ." [char] " parse postpone (.") dup c, swap over here swap cmove allot ; immediate

\ : spaces ( n -- ) 0 max ?dup if 0 do space loop then ;
    
.( ... end of core.f ) cr