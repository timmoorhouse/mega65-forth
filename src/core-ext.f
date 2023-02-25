
\ TODO latest won't work with :noname
: :noname ( -- xt ) align ] 
    \ $80 , \ end marker flag, zero length name \ TODO skip these??
    here ['] : @ , ; \ assumes the code field of : is DO_COLON

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

: tuck swap over ;

: u> swap u< ;

\ : pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr