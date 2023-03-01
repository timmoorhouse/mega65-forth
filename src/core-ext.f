
: \ ( "ccc<eol>" -- ) 13 parse 2drop ; immediate ( k-return is not available yet )

\ The following words are implemented internally:                             
\
\ 0<> 0> 2>R 2R> 2R@ :NONAME <> ?DO DEFER DEFER FALSE NIP PARSE PARSE-NAME    
\ PICK REFILL RESTORE-INPUT ROLL S\" SAVE-INPUT SOURCE-ID TRUE UNUSED VALUE   

: defer@ ( xt1 -- xt2 ) >body @ ;

: defer! ( xt2 xt1 -- ) >body ! ;

\ *************************************************************************** 

\ .( see core.f

\ .R see core.f

\ see also do in core.f
: ?do ( C: -- do-sys ) ( n1|u1 n2|u2 -- ) ( R: loop-sys ) 
  postpone (?do) 0 , here ( 3 ) ; immediate ( compile-only )

: action-of ( "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer@
   else
     ' defer@
   then ; immediate
   
\ AGAIN see core.f

: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ TODO c"
: c" ( "ccc<quote>" -- ) ( -- c-addr ) 
  [char] " parse postpone (c") ( addr u ) dup c, swap over here swap cmove allot ; immediate compile-only

: case ( C: -- case-sys ) ( -- ) 0 ; immediate compile-only

: compile, ( xt -- ) , ; ( compile-only )

\ TODO From discussion in ANSI A.3.2.3.2:
\     : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; IMMEDIATE
: endcase ( C: case-sys1 of-sys -- case-sys2 ) ( -- )
    postpone drop
    ?dup if 0 do here over - swap ! loop then \ TODO ?do
    ; immediate compile-only

\ TODO duplication with then
\ TODO From discussion in ANSI A.3.2.3.2:
\   : ENDOF >R POSTPONE ELSE R> ; IMMEDIATE
: endof postpone branch here rot 1+ rot 0 , \ branch to endcase
  here over - swap ! ; immediate compile-only \ branch of chained condition checks

: erase ( addr u ) 0 fill ;

: hex ( -- ) 16 base ! ;

: holds ( c-addr u -- ) begin dup while 1- 2dup + c@ hold repeat 2drop ;

: is ( xt "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer!
   else
     ' defer!
   then ; immediate

\ TODO marker
: marker ( "<spaces>name" -- ) ( -- ) create does> ;

\ TODO From discussion in ANSI A.3.2.3.2:
\     : OF 1+ >R POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP R> ; IMMEDIATE
: of ( C: -- of-sys ) ( x1 x1 -- | x1 ) postpone over postpone = postpone 0branch here 0 , 
    postpone drop ; immediate compile-only

\ PAD see core.f

\ TODO s\"
\ see http://www.forth200x.org/escaped-strings.html
: s\" ( "ccc<quote>" -- ) ( -- c-addr u ) postpone s" ; immediate

: to ( x "<spaces>name" -- ) 
  state @ if
    postpone ['] postpone >body postpone !
  else
    ' >body ! 
  then ; immediate

: tuck ( x1 x2 -- x2 x1 x2 ) swap over ;

\ U.R see core.f

: u> ( u1 u2 -- flag ) swap u< ;

: within ( n1|u1 n2|u2 n3|u3 -- flag ) over - >r - r> u< ;

.( ... end of core-ext.f ) cr