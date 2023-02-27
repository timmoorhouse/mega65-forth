
: literal postpone (literal) , ; immediate

: char parse-name drop c@ ; immediate

: [char] postpone char postpone literal ; immediate

: ( [char] ) parse 2drop ; immediate
( TODO allow multiline comments when parsing from a file )

: .( [char] ) parse type ; immediate ( CORE-EXT )

.( Starting bootstrap... ) cr

( The following words are implemented internally:                             )
(                                                                             )
( ! * + +! +LOOP , - . / 0< 0= 1+ 1- 2* 2/ 2DROP 2DUP 2OVER 2SWAP : ; < = >IN )
( >NUMBER >R ?DUP @ ABORT ACCEPT ALIGN ALIGNED ALLOT AND BASE BL C! C@        )
( CONSTANT COUNT CR CREATE DECIMAL DEPTH <DO> DOES> DROP DUP EMIT EVALUATE    )
( EXECUTE EXIT FILL HERE I IMMEDIATE INVERT J KEY LEAVE <LOOP> LSHIFT MIN     )
( NEGATE OR OVER POSTPONE QUIT R> R@ ROT RSHIFT <S"> SOURCE SPACE STATE SWAP  )
( TYPE U< UM* UM/MOD UNLOOP VARIABLE WORD XOR [ ]                             )

( *************************************************************************** )
( * internal helper words                                                   * )
( *************************************************************************** )

: back ( ?comp ) here - , ; ( Resolve backward branch ) ( TODO REMOVE? )

( *************************************************************************** )
( * control flow                                                            * )
( *************************************************************************** )

( FIG control flow tags   )
( 1 begin...[again|until] )
( 2 if...                 )
( 3 do... [loop|+loop]    )

( TODO use CS-PICK/CS-ROLL )

( Marks the origin of an unconditional forward branch )
: ahead ( ?comp ) postpone branch here 0 , ( 2 ) ; immediate ( TOOLS-EXT )

( Marks the origin of a conditional forward branch )
: if ( ?comp ) postpone 0branch here 0 , ( 2 ) ; immediate

( Resolves an IF or AHEAD )
: then ( ?comp 2 ?pairs ) here over - swap ! ; immediate

: else ( ?comp 2 ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate

( Marks the destination of a backwards branch )
: begin ( ?comp ) here ( 1 ) ; immediate

( Resolves a BEGIN with an unconditional backwards branch )
: again ( ?comp 1 ?pairs ) postpone branch back ; immediate ( TODO CORE-EXT )

( Resolves a BEGIN with a conditional backwards branch )
: until ( ?comp 1 ?pairs ) postpone 0branch back ; immediate

: while postpone if swap ; immediate

: repeat postpone again postpone then ; immediate

: do postpone (do) 0 , here ( 3 ) ; immediate

: loop ( 3 ?pairs ) postpone (loop) dup 2 - here 2 + swap ! back ; immediate

: +loop ( 3 ?pairs ) postpone (+loop) dup 2 - here 2 + swap ! back ; immediate

( *************************************************************************** )
( * more internal helper words                                              * )
( *************************************************************************** )

: +- 0< if negate then ; ( TODO REMOVE )

variable hld ( TODO can we remove this? )

: m/mod ( ud1 u2 -- u3 ud4 ) >r 0 r@ um/mod r> swap >r um/mod r> ; ( TODO just for # so far )

( *************************************************************************** )

: ' ( "<spaces>name" -- xt ) parse-name forth-wordlist search-wordlist drop ;

: 2! swap over ! 2+ ! ;

: 2@ dup 2+ @ swap @ ;

( TODO )
( : abort" ; )

: > swap < ;

: >body ( xt -- a-addr ) 2+ ;

: abs dup +- ;
( : abs dup 0< if negate then ; )

: c, here c! 1 allot ;

: cell+ ( a-addr_1 -- a-addr_2 ) 2+ ;

: cells ( n_1 -- n_2 ) 2* ;

: char+ ( c-addr_1 -- c-addr_2 ) 1+ ;

: chars ( n_1 -- n_2 ) ;

: dabs dup 0< if dnegate then ; ( DOUBLE )

( TODO might be simpler to do this one in assembler )
: environment? 2drop false ;
( TODO /COUNTED-STRING )
( TODO /HOLD )
( TODO /PAD )
( TODO ADDRESS-UNIT-BITS )
( TODO FLOORED )
( TODO MAX-CHAR )
( TODO MAX-D )
( TODO MAX-N )
( TODO MAX-U )
( TODO MAX-UD )
( TODO RETURN-STACK-CELLS )
( TODO STACK-CELLS )

: find dup count forth-wordlist search-wordlist dup if rot drop then ;

: m* 2dup xor >r abs swap abs um* r> 0< if dnegate then ;

: max ( n_1 n_2 -- n_3 ) 2dup < if swap then drop ;

( TODO cmove is in STRING but move is in CORE - make move the native one )
: move ( src dst len -- ) >r 2dup < r> swap if cmove> else cmove then ;

: recurse latestxt , ; immediate

( TODO use sliteral! )
( TODO alignment after string? )
( TODO s" broken when interpreting )
: s" [char] " parse postpone (s") dup c, swap over here swap cmove allot ; immediate
: ." postpone s" postpone type ; immediate

: s>d dup 0< ;

: spaces ( n -- ) 0 max ?dup if 0 do space loop then ; ( TODO use ?do )

: * 2dup 2>r abs swap abs um* drop r> +- r> +- ; ( TODO SLOW )

: sm/rem over >r >r dabs r@ abs um/mod r> r@ xor +- swap r> +- swap ; ( TODO )

: fm/mod ( d n -- rem quot )
  dup >r sm/rem
  ( if the remainder is not zero and has a different sign than the divisor )
  over dup 0<> swap 0< r@ 0< xor and if
    1- swap r> + swap
  else
    r> drop
  then ;

( For floored               )
( : /mod >r s>d r> fm/mod ; )
( : */mod >r m* r> fm/mod ; )

( For symmetric )
: /mod >r s>d r> sm/rem ; ( TODO )
: */mod >r m* r> sm/rem ; ( TODO )

: / /mod nip ; ( TODO - depends on nip from CORE-EXT )
: mod /mod drop ; ( TODO )
: */ */mod nip ; ( TODO - depends on nip from CORE-EXT )

: hold ( char -- ) -1 hld +! hld @ c! ; ( hmm ... this goes backwards.  OK with the gap, but might want to change this )
: <# ( -- ) pad hld ! ;
: #> ( xd -- c-addr u ) 2drop hld @ pad over - ;
: # ( ud1 -- ud2 ) base @ m/mod rot 9 over < if 7 + then '0' + hold ; ( TODO 7 is the gap between '9' and 'A' )
: sign ( n -- ) 0< if '-' hold then ;
: #s ( ud1 -- ud2 ) begin # 2dup or 0= until ; ( TODO d0= instead of 2dup or 0= )
: d.r ( d n -- ) >r swap over dabs <# #s rot sign #> r> over - spaces type ; ( DOUBLE )
: d. 0 d.r space ; ( DOUBLE )
: .r >r s>d r> d.r ; ( CORE-EXT )
: u.r ( u n -- ) >r 0 <# #s #> r> over - spaces type ; ( CORE-EXT )
: u. 0 u.r space ;
: . s>d d. ;

: ['] ' postpone literal ; immediate

.( ... end of core.f ) cr