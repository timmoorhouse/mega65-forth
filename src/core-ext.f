
: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ TODO c"

\ 5 case
\    1 of ... endof
\    2 of ... endof
\    3 of ... endof
\    ...
\  endcase
: case 0 ; immediate

\ TODO From discussion in ANSI A.3.2.3.2:
\     : OF 1+ >R POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP R> ; IMMEDIATE
: of postpone over postpone = postpone 0branch here 0 , 
    postpone drop ; immediate

\ TODO duplication with then
\ TODO From discussion in ANSI A.3.2.3.2:
\   : ENDOF >R POSTPONE ELSE R> ; IMMEDIATE
: endof postpone branch here rot 1+ rot 0 , \ branch to endcase
  here over - swap ! ; immediate \ branch of chained condition checks

\ TODO From discussion in ANSI A.3.2.3.2:
\     : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; IMMEDIATE
: endcase 
    postpone drop
    ?dup if 0 do here over - swap ! loop then \ TODO ?do
    ; immediate

\ see also do in core.f
: ?do postpone (?do) 0 , here ( 3 ) ; immediate

: compile, , ;

: defer@ ( xt1 -- xt2 ) >body @ ;

: defer! ( xt2 xt1 -- ) >body ! ;

: is ( xt "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer!
   else
     ' defer!
   then ; immediate

: action-of ( "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer@
   else
     ' defer@
   then ; immediate

: erase ( c-addr u ) 0 fill ;

: hex 16 base ! ;

: holds ( addr u -- ) begin dup while 1- 2dup + c@ hold repeat 2drop ;

: to ( x "<spaces>name" -- ) 
  state @ if
    postpone ['] postpone >body postpone !
  else
    ' >body ! 
  then ; immediate

: tuck swap over ;

: u> swap u< ;

: within ( test low high -- flag ) over - >r - r> u< ;

\ TODO marker

\ : pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr