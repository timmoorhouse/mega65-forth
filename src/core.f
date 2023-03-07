
: literal postpone (literal) , ; immediate compile-only

: char parse-name drop c@ ;

: [char] char postpone literal ; immediate compile-only

: ( [char] ) parse 2drop ; immediate
( "ccc<paren>" -- )
( TODO allow multiline comments when parsing from a file )
( TODO should char throw if parse-name gives 0 length ? )

: .( ( "ccc<paren>" -- ) [char] ) parse type ; immediate ( CORE-EXT )

( The following words are implemented internally:                             )
(                                                                             )
( ! * + +! +LOOP , - . / 0< 0= 1+ 1- 2* 2/ 2DROP 2DUP 2OVER 2SWAP : ; < = >IN )
( >NUMBER >R ?DUP @ ACCEPT ALIGN ALIGNED ALLOT AND BASE BL C! C@ CONSTANT     )
( COUNT CR CREATE DECIMAL DEPTH <DO> DOES> DROP DUP EMIT ENVIRONMENT?         )
( EVALUATE EXECUTE EXIT FILL HERE I IMMEDIATE INVERT J KEY LEAVE <LOOP>       )
( LSHIFT NEGATE OR OVER POSTPONE QUIT R> R@ ROT RSHIFT <S"> SOURCE STATE SWAP )
( TYPE U< UM* UM/MOD UNLOOP VARIABLE XOR [ ]                                  )

( *************************************************************************** )
( * internal helper words                                                   * )
( *************************************************************************** )

( Resolve backward branch ) 
: back ( -- )  
  here - , ; compile-only ( TODO REMOVE? )

( *************************************************************************** )
( * control flow                                                            * )
( *************************************************************************** )

( FIG control flow tags   )
( 1 begin...[again|until] )
( 2 if...                 )
( 3 do... [loop|+loop]    )

( TODO use CS-PICK/CS-ROLL )

( Marks the origin of an unconditional forward branch )
: ahead ( C: -- orig ) ( -- ) 
  postpone branch here 0 , ( 2 ) ; immediate compile-only ( TOOLS-EXT )

( Marks the origin of a conditional forward branch )
: if ( C: -- orig ) ( x -- )
  postpone 0branch here 0 , ( 2 ) ; immediate compile-only

( Resolves a forward branch, from IF or AHEAD )
: then ( C: orig -- ) ( -- )
  ( ?pairs ) here over - swap ! ; immediate compile-only 

: else ( C: orig1 -- orig2 ) ( -- )
  ( ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate compile-only

( Marks the destination of a backwards branch )
: begin ( C: -- dest ) ( -- ) 
  here ( 1 ) ; immediate compile-only

( Resolves an unconditional backwards branch to BEGIN )
: again ( C: dest -- ) ( -- )
  ( 1 ?pairs ) postpone branch back ; immediate compile-only ( TODO CORE-EXT )

( Resolves a conditional backwards branch to BEGIN )
: until ( C: dest -- ) ( x -- ) 
  ( 1 ?pairs ) postpone 0branch back ; immediate compile-only

: while ( C: dest -- orig dest ) ( x -- )
  postpone if swap ; immediate compile-only

: repeat ( C: orig dest -- ) ( -- )
  postpone again postpone then ; immediate compile-only

: do ( C: -- do-sys ) ( n1|u2 n2|u2 -- ) ( R: -- loop-sys )
  postpone (do) 0 , here ( 3 ) ; immediate compile-only

: loop ( C: do-sys -- ) ( -- ) ( R: loop-sys1 -- | loop-sys2 )
  ( 3 ?pairs ) postpone (loop) dup 2 - here 2 + swap ! back ; immediate compile-only

: +loop ( C: do-sys -- )
  ( 3 ?pairs ) postpone (+loop) dup 2 - here 2 + swap ! back ; immediate compile-only

( *************************************************************************** )
( * more internal helper words and commonly used things                     * )
( *************************************************************************** )

: c, ( char -- ) here c! 1 allot ;

: negate ( n1 -- n2 ) invert 1+ ;

: +- ( n1 n2 -- n3 ) 0< if negate then ; ( TODO REMOVE )

variable hld ( TODO can we remove this? )

: m/mod ( ud1 u2 -- u3 ud4 ) >r 0 r@ um/mod r> swap >r um/mod r> ; ( TODO just for # so far )

( counted string literal )
: csliteral ( c-addr u -- ) ( -- c-addr )
  postpone (csliteral) dup c, swap over here swap cmove allot
  ; immediate compile-only

( string literal )
( TODO this only works for len <= 255 )
: sliteral ( c-addr u -- ) ( -- c-addr u ) 
  postpone csliteral postpone count
  ; immediate compile-only ( STRING )

( *************************************************************************** )

( TODO throw E_INVALID_NAME on error cases? )
: ' ( "<spaces>name" -- xt ) parse-name find-name name>interpret ;

: ['] ( "<spaces>name" -- ) ( -- xt ) ' postpone literal ; immediate compile-only

: 2! ( x1 x2 a-addr -- ) swap over ! 2+ ! ;

: 2@ ( a-addr -- x1 x2 ) dup 2+ @ swap @ ;

: > ( n1 n2 -- flag ) swap < ;

: >body ( xt -- a-addr ) 2+ ;

: abort ( i*x -- ) ( R: j*x -- ) -1 throw ;

( TODO the message should not get displayed if caught ??? )
: abort" ( "ccc<quote>" -- )
  postpone if 
    [char] " parse postpone sliteral postpone type ( TODO postpone ." )
    -2 postpone literal postpone throw
  postpone then ; immediate compile-only

: abs ( n -- u ) dup +- ;
( : abs dup 0< if negate then ; )

: cell+ ( a-addr1 -- a-addr2 ) 2+ ;

: cells ( n_1 -- n_2 ) 2* ;

: char+ ( c-addr1 -- c-addr2 ) 1+ ;

: chars ( n1 -- n2 ) ;

: dabs dup 0< if dnegate then ; ( DOUBLE )

: defer@ ( xt1 -- xt2 ) >body @ ; ( CORE-EXT )

: defer! ( xt2 xt1 -- ) >body ! ; ( CORE-EXT )

: find ( c-addr -- c-addr 0 | xt 1 | xt -1 ) 
  dup count forth-wordlist search-wordlist dup if rot drop then ;

: is ( xt "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer!
   else
     ' defer!
   then ; immediate ( CORE-EXT )

: m* ( n1 n2 -- d ) 2dup xor >r abs swap abs um* r> 0< if dnegate then ;

: min ( n1 n2 -- n3 ) 2dup > if swap then drop ;

: max ( n1 n2 -- n3 ) 2dup < if swap then drop ;

: move ( addr1 addr2 u -- ) >r 2dup < r> swap if cmove> else cmove then ;

( TODO the 68 is enough space for WORD and HOLD transient regions )
: pad ( -- c-addr ) here 68 + ; ( CORE-EXT ) 

: recurse ( -- ) latestxt , ; immediate

: s" ( "ccc<quote>" -- ) ( -- c-addr u ) 
  [char] " parse 
  state @ if
    postpone sliteral
  else
    sbuf swap 2dup 2>r cmove 2r> ( TODO klunky )
  then ; immediate

: ." ( "ccc<quote>" -- ) postpone s" postpone type ; immediate compile-only

: s>d ( x -- d ) dup 0< ;

: space ( -- ) bl emit ;

: spaces ( n -- ) 0 max ?dup if 0 do space loop then ; ( TODO use ?do )

: * ( n1|u1 n2|u2 -- n3|u3 ) 2dup 2>r abs swap abs um* drop r> +- r> +- ; ( TODO SLOW )

: sm/rem ( d1 n1 -- n2 n3 ) over >r >r dabs r@ abs um/mod r> r@ xor +- swap r> +- swap ; ( TODO )

: fm/mod ( d1 n1 -- n2 n3 )
  dup >r sm/rem
  ( if the remainder is not zero and has a different sign than the divisor )
  over dup 0<> swap 0< r@ 0< xor and if
    1- swap r> + swap
  else
    r> drop
  then ;

: /mod ( n1 n2 -- n3 n4 ) >r s>d r> sm/rem ; ( TODO )

: */mod ( n1 n2 n3 -- n4 n5 ) >r m* r> sm/rem ; ( TODO )

: / ( n1 n2 -- n3 ) /mod nip ; ( TODO - depends on nip from CORE-EXT )

: mod ( n1 n2 -- n3 ) /mod drop ; ( TODO )

: */ ( n1 n2 n3 -- n4 ) */mod nip ; ( TODO - depends on nip from CORE-EXT )

( TODO check for buffer overflow, throw -17 )
( hmm ... this goes backwards.  OK with the gap, but might want to change this )
: hold ( char -- ) -1 hld +! hld @ c! ; 

: <# ( -- ) pad hld ! ;

: #> ( xd -- c-addr u ) 2drop hld @ pad over - ;

( TODO 7 is the gap between '9' and 'A' )
: # ( ud1 -- ud2 ) base @ m/mod rot 9 over < if 7 + then '0' + hold ; 

: sign ( n -- ) 0< if '-' hold then ;

: #s ( ud1 -- ud2 ) begin # 2dup or 0= until ; ( TODO d0= instead of 2dup or 0= )

: d.r ( d n -- ) >r swap over dabs <# #s rot sign #> r> over - spaces type ; ( DOUBLE )

: d. ( d -- ) 0 d.r space ; ( DOUBLE )

: .r ( n1 n2 -- ) >r s>d r> d.r ; ( CORE-EXT )

: u.r ( u n -- ) >r 0 <# #s #> r> over - spaces type ; ( CORE-EXT )

: u. ( u -- ) 0 u.r space ;

:noname ( n -- ) s>d d. ; is .

: word ( char "<chars>ccc<char>" -- c-addr ) 
  (parse-name) dup here c! here 1+ swap cmove here ;

.( ... END OF CORE.F ) cr