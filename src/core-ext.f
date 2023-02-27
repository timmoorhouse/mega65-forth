
( The following words are implemented internally:                             )

( 0<> 0> 2>R 2R> 2R@ :NONAME <> ?DO DEFER DEFER FALSE NIP PARSE PARSE-NAME    )
( PICK REFILL RESTORE-INPUT ROLL S\" SAVE-INPUT SOURCE-ID TRUE UNUSED VALUE   )

: \ 13 parse 2drop ; immediate ( TODO K-RETURN? )

: defer@ ( xt1 -- xt2 ) >body @ ;

: defer! ( xt2 xt1 -- ) >body ! ;

( *************************************************************************** )

\ .( see core.f

\ .R see core.f

\ see also do in core.f
: ?do postpone (?do) 0 , here ( 3 ) ; immediate

: action-of ( "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer@
   else
     ' defer@
   then ; immediate
   
\ AGAIN see core.f

: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ TODO c"

: case 0 ; immediate

: compile, , ;

\ TODO From discussion in ANSI A.3.2.3.2:
\     : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; IMMEDIATE
: endcase 
    postpone drop
    ?dup if 0 do here over - swap ! loop then \ TODO ?do
    ; immediate

\ TODO duplication with then
\ TODO From discussion in ANSI A.3.2.3.2:
\   : ENDOF >R POSTPONE ELSE R> ; IMMEDIATE
: endof postpone branch here rot 1+ rot 0 , \ branch to endcase
  here over - swap ! ; immediate \ branch of chained condition checks

: erase ( c-addr u ) 0 fill ;

: hex 16 base ! ;

: holds ( addr u -- ) begin dup while 1- 2dup + c@ hold repeat 2drop ;

: is ( xt "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer!
   else
     ' defer!
   then ; immediate

\ TODO marker

\ TODO From discussion in ANSI A.3.2.3.2:
\     : OF 1+ >R POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP R> ; IMMEDIATE
: of postpone over postpone = postpone 0branch here 0 , 
    postpone drop ; immediate

\ PAD see core.f

\ TODO s\"

: to ( x "<spaces>name" -- ) 
  state @ if
    postpone ['] postpone >body postpone !
  else
    ' >body ! 
  then ; immediate

: tuck swap over ;

\ U.R see core.f

: u> swap u< ;

\ TODO unused

: within ( test low high -- flag ) over - >r - r> u< ;

.( ... end of core-ext.f ) cr