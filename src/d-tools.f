
internals-wordlist current !

64 constant c/l     \ characters per line, used by words

: id. ( nt -- ) name>string type ;

\ TODO use a :noname ...
: print-name ( nt -- u ) 
    out c@ 
    dup if space then
    c/l > if cr then
    id.
    true ;

: find-xt 
  \ ( xt 0 nt -- xt 0 true ) if not found
  \ ( xt 0 nt -- xt nt false ) if found
  nip dup name>xt ( xt1 nt xt2 )
  2 pick = if false else drop 0 true then
  ;

\ : words ( -- ) get-order 0 ?do ['] print-name swap traverse-wordlist loop ;
: >name ( xt -- nt | 0 )
  \ TODO
  \ - skip over possible alignment byte
  \ - crawl backwards over at most 31 printable chars
  \ - stop if we hit something where lower 5 bits match length
  \ OR
  \ - traverse each wordlist looking for a match
  >r get-order r> 0 rot 0 ?do 
    ( widn ... widi xt 0|nt ) \ TODO !!!!!!!!!
    rot ['] find-xt swap traverse-wordlist 
  loop
  ( xt 0|nt )
  nip
  ;

\ TODO handle does>
: colon-see ( xt -- )
  dup . ." :" cr 2+
  40 0 do
    dup .
    dup @ case
      \ Relative branches
      ['] branch      of ." branch "     2+ dup dup @ + . cr endof
      ['] 0branch     of ." 0branch "    2+ dup dup @ + . cr endof
      ['] (loop)      of ." loop "       2+ dup dup @ + . cr endof 
      ['] (+loop)     of ." +loop "      2+ dup dup @ + . cr endof
      \ Others ...
      ['] (do)        of ." do "         2+ dup @ .       cr endof
      ['] (?do)       of ." ?do "        2+ dup @ .       cr endof
      ['] (cliteral)  of dup 2+ c@ . 1+ cr endof
      ['] (literal)   of 2+ dup @ '$' emit . cr endof
      ['] (csliteral) of 2+ dup dup c@ + 1+ swap s\" c\" " type count type '"' emit cr endof
      ['] (2literal)  of ." (2literal)"  cr endof \ TODO the literal
      ['] (;)         of ." ;" cr leave endof
      ['] (;code)     of 
        \ TODO check for jsr/$20 (does>) if so, continue otherwise leave
        ." TODO ;code " cr 
      endof 
      \ ['] forth of ." TODO DOES>" cr endof \ TODO
      \ TODO ;code
      dup >name ?dup if name>string type else dup u. then cr
    endcase
    2+
  loop
  drop
  ;

forth-wordlist current !

\ *************************************************************************** 

\ TODO if base is 10 used signed output, otherwise unsigned?
: .s ( -- ) '<' emit depth 0 u.r '>' emit space depth if 0 depth 2- do i pick . -1 +loop then ;

: ? ( a-addr -- ) @ . ;

: dump ( addr u -- ) \ u is number of lines to display
    base @ >r hex cr ( addr u ) ( R: base )
    0 do   ( addr )
        16 ( addr u2 )
        over 4 u.r 
        2dup over + swap do space i c@ 2 u.r loop
        \ space ':' emit space
        \ 2dup over + swap do i c@ emit loop \ TODO would need a check for printable chars
        cr
        +
    loop
    drop r> base ! ;

\ following gforth ...
: xt-see ( xt -- ) 
    base @ >r hex
    dup @ case
    ['] :     @ of colon-see endof
    ['] false @ of 2+ @ . ." constant" cr endof
    ['] >in   @ of 2+ @ . ." variable" cr endof
    \ TODO value
    \ TODO 2constant
    \ TODO 2variable
    \ TODO does
    \ TODO defer
        >r 3 dump r>
    endcase
    \ dup 4 u.r space ." TODO - xt-see "
    r> base ! ;

\ TODO
\ - check code field
\   - DO_VARIABLE
\   - DO_CONSTANT
\   - DO_COLON
\     - print ': <name>'
\     - scan a word at a time, resolving word name until we hit (;)
\     - will need special handling for words like LITERAL, DO, BRANCH, 0BRANCH
\     - might not be able to transform control structures back to IF, etc (that should be ok?)
\     - will need to change builtins to only have a single (;)
\     - limit to maximum length in case we get out of sync
\     - show address of everything and hex dump (like acme listing)
\   - DO_DOES
\     - might not be able to do anything sensible for this case?
: see ( "<spaces>name" -- )
    base @ >r hex cr
    parse-name find-name ?dup if
        dup 4 u.r dup name>string space type 
        dup ?immediate    if space ." immediate"    then
        dup ?compile-only if space ." compile-only" then
        cr
        name>xt xt-see
    then r> base ! ;

: words ( -- ) get-order 0 ?do ['] print-name swap traverse-wordlist loop ;

.( ... end of d-tools.f ) cr