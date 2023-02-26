
: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ 5 case
\    1 of ... endof
\    2 of ... endof
\    3 of ... endof
\    ...
\  endcase
: case 0 ; immediate

: of postpone over postpone = postpone 0branch here 0 , 
    postpone drop ; immediate

\ TODO duplication with then
: endof postpone branch here rot 1+ rot 0 , \ branch to endcase
  here over - swap ! ; immediate \ branch of chained condition checks

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

: tuck swap over ;

: u> swap u< ;

: within ( test low high -- flag ) over - >r - r> u< ;

\ : pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr