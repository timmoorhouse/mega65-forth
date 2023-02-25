
: compile, , ;

: defer@ ( xt1 -- xt2 ) >body @ ;

: defer! ( xt2 xt1 -- ) >body ! ;

\ : is ( xt "<spaces>name" -- )
\    state @ if
\      postpone ['] postpone defer!
\    else
\      ' defer!
\    then ; immediate

\ TODO references implementation doesn't have the postpone ...
\ problem with our ' ? 
: is ( xt "<spaces>name" -- ) 
    ' defer! ; immediate

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