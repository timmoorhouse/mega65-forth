
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

\ TODO use AHEAD
: else ( ?comp 2 ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate
\ : ELSE     2  ?PAIRS  COMPILE  BRANCH  HERE  0  ,  SWAP  2  [COMPILE]  ENDIF  2  ; IMMEDIATE

\ ***************************************************************************

\ : spaces ( n -- ) 0 max ?dup if 0 do space loop then ;
    
.( end of core.f )