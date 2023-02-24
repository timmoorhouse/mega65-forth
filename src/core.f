
: literal postpone (literal) , ; immediate

: [char] parse-name drop c@ postpone literal ; immediate

: ( [char] ) parse 2drop ; immediate
( TODO allow multiline comments when parsing from a file )

: \ 13 parse 2drop ; immediate

: .( [char] ) parse type ; immediate
( we cheated and put this in core instead of core-ext )

.( Starting bootstrap... ) cr

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

\ TODO change swap to 1 cs-roll ?

: else ( ?comp 2 ?pairs ) postpone ahead swap ( 2 ) postpone then ( 2 ) ; immediate

: while postpone if 1 cs-roll ; immediate

: repeat postpone again postpone then ; immediate

\ TODO do ... loop is wrong
\ need to find where to exit to in a leave
\ - have address of end of loop inline after the (do)
\ - (do) copies end address to return stack
\ - (loop), (+loop) pulls from return stack on exit
\ - leave pulls from return stack and sets I

: do postpone (do) 0 , here ( 3 ) ; immediate

: loop ( 3 ?pairs ) postpone (loop) dup 2 - here 2 + swap ! back ; immediate

: +loop ( 3 ?pairs ) postpone (+loop) dup 2 - here 2 + swap ! back ; immediate

\ : WHILE   [COMPILE]  IF  2+  ;    IMMEDIATE
\ : REPEAT   >R  >R  [COMPILE]  AGAIN  R>  R>  2  -  [COMPILE]  ENDIF  ;  IMMEDIATE
       
\ ***************************************************************************

: ' ( "<spaces>name" -- xt ) parse-name forth-wordlist search-wordlist drop ; immediate

: > swap < ;

: >body ( xt -- a-addr ) 2+ ;

\ : abs ( n -- u ) dup 0< if negate then ;

: c, here c! 1 allot ;

: cell+ ( a-addr_1 -- a-addr_2 ) 2+ ;

: cells ( n_1 -- n_2 ) 2* ;

: char+ ( c-addr_1 -- c-addr_2 ) 1+ ;

: chars ( n_1 -- n_2 ) ;

: m* 2dup xor >r abs swap abs um* r> 0< if dnegate then ;

\ TODO choose cmove or cmove> ?
: move cmove ;

\ TODO use sliteral!
\ TODO alignment after string?
: s" [char] " parse postpone (s") dup c, swap over here swap cmove allot ; immediate
: ." postpone s" postpone type ; immediate

: s>d dup 0< ;

: 2! swap over ! cell+ ! ;

: 2@ dup cell+ @ swap @ ;

: ['] postpone ' postpone literal ; immediate

: recurse latest name>interpret , ; immediate

\ : spaces ( n -- ) 0 max ?dup if 0 do space loop then ; \ TODO use ?do

: +- 0< if negate then ; \ TODO REMOVE

: * 2dup 2>r abs swap abs um* drop r> +- r> +- ; \ TODO SLOW

: sm/rem over >r >r dabs r@ abs um/mod r> r@ xor +- swap r> +- swap ; \ TODO

\ For floored
\ : /mod >r s>d r> fm/mod ; \ TODO
\ : */mod >r m* r> fm/mod ; \ TODO

\ For symmetric
: /mod >r s>d r> sm/rem ; \ TODO
: */mod >r m* r> sm/rem ; \ TODO

: / /mod swap drop ; \ TODO
: mod /mod drop ; \ TODO
: */ */mod swap drop ; \ TODO

\ TODO THIS IS WRONG
\ : fm/mod over >r >r dabs r@ abs um/mod r> r@ xor +- swap r> +- swap ;

\ ***************************************************************************

\ These look OK
variable hld
: hold ( char -- ) -1 hld +! hld @ c! ; \ hmm ... this goes backwards.  OK with the gap, but might want to change this
: <# ( -- ) pad hld ! ;
: #> ( xd -- c-addr u ) 2drop hld @ pad over - ;
: m/mod ( ud1 u2 -- u3 ud4 ) >r 0 r@ um/mod r> swap >r um/mod r> ; \ TODO just for # so far
: # ( ud1 -- ud2 ) base @ m/mod rot 9 over < if 7 + then '0' + hold ; \ TODO 7 is the gap between '9' and 'A'
: sign ( n -- ) 0< if '-' hold then ;
: #s ( ud1 -- ud2 ) begin # 2dup or 0= until ; \ TODO d0= instead of 2dup or 0=
: d.r ( d n -- ) >r swap over dabs <# #s rot sign #> r> over - spaces type ;
: d. 0 d.r space ;
: .r >r s>d r> d.r ;
: u.r ( ud n -- ) >r <# #s #> r> over - spaces type ;
: u. 0 u.r space ;
\ : . s>d d. ;

.( ... end of core.f ) cr