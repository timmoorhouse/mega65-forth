
: immediate latest 2+ dup c@ $80 or swap c! ;

: compile-only latest 2+ dup c@ $40 or swap c! ;

: literal postpone (literal) , ; immediate compile-only

: char parse-name drop c@ ;

: [char] char postpone literal ; immediate compile-only

: ( [char] ) parse 2drop ; immediate

( This file needs to contain everything necessary for S" and INCLUDED to work )
( Everything else can be postponed to stage 2 )
( Not all of the control flow stuff is necessary but for sanity's sake we keep it together )

( FIG control flow tags   )
( 1 begin...[again|until] )
( 2 if...                 )
( 3 do... [loop|+loop]    )

( Resolve backward branch ) 
: back here - , ; compile-only ( TODO REMOVE? )

( Marks the origin of an unconditional forward branch )
: ahead postpone branch here 0 , ( 2 ) ; immediate compile-only

( Marks the origin of a conditional forward branch )
: if postpone 0branch here 0 , ( 2 ) ; immediate compile-only

( Resolves a forward branch, from IF or AHEAD )
: then ( ?pairs ) here over - swap ! ; immediate compile-only 

: else ( ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate compile-only

( Marks the destination of a backwards branch )
: begin here ( 1 ) ; immediate compile-only

( Resolves an unconditional backwards branch to BEGIN )
: again ( 1 ?pairs ) postpone branch back ; immediate compile-only

( Resolves a conditional backwards branch to BEGIN )
: until ( 1 ?pairs ) postpone 0branch back ; immediate compile-only

: while postpone if swap ; immediate compile-only

: repeat postpone again postpone then ; immediate compile-only

: do postpone (do) 0 , here ( 3 ) ; immediate compile-only

: loop ( 3 ?pairs ) postpone (loop) dup 2 - here 2 + swap ! back ; immediate compile-only

: +loop ( 3 ?pairs ) postpone (+loop) dup 2 - here 2 + swap ! back ; immediate compile-only

: .( ( "ccc<paren>" -- ) [char] ) parse type ; immediate

: c, ( char -- ) here c! 1 allot ;

( counted string literal )
: csliteral ( c-addr u -- ) ( -- c-addr )
  postpone (csliteral) dup c, swap over here swap cmove allot
  ; immediate compile-only

( TODO this only works for len <= 255 )
: sliteral ( c-addr u -- ) ( -- c-addr u ) 
  postpone csliteral postpone count
  ; immediate compile-only

: s" ( "ccc<quote>" -- ) ( -- c-addr u ) 
  [char] " parse 
  state @ if
    postpone sliteral
  else
    sbuf swap 2dup 2>r cmove 2r> ( TODO klunky )
  then ; immediate

: ' parse-name find-name name>interpret ;

: ['] ' postpone literal ; immediate compile-only

: >body 2+ ;

: to state @ if
    postpone ['] postpone >body postpone !
  else
    ' >body !
  then ; immediate
 
( ' : @  is to obtain DO_COLON )
: :noname align here to latestxt ] 0 to latest here ['] : @ , ;

1 constant r/o

: included r/o open-file if -38 throw then 
  >r r@ include-file r> close-file drop ;

.( ... BOOTSTRAP STAGE 1 COMPLETE ) cr
