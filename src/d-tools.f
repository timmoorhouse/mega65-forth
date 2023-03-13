
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

: >name ( xt -- nt | 0 )
  \ TODO
  \ - skip over possible alignment byte
  \ - crawl backwards over at most 31 printable chars
  \ - stop if we hit something where lower 5 bits match length
  \ OR
  \ - traverse each wordlist looking for a match
  ;

: colon-see ( xt -- )
  cr ." TODO do_colon" cr
  20 0 do
    dup . space
    dup @ case
        ( >r ) dup . cr ( r> )
    endcase
    2+
  loop
  drop
  ;

forth-wordlist current !

\ *************************************************************************** 

\ TODO if base is 10 used signed output, otherwise unsigned?
: .s '<' emit depth 0 u.r '>' emit space depth if 0 depth 2- do i pick . -1 +loop then ;

: ? @ . ;

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
    ['] false @ of ." TODO do_constant" cr 1 dump endof
    ['] >in   @ of ." TODO do_variable" cr 1 dump endof
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
        .s cr
        dup 4 u.r space ':' emit space dup name>string type
        \ TODO show name, flags
        dup name>interpret xt-see
        5 spaces ';' emit
        ?immediate if ." immediate" then
        cr
    then r> base ! ;

: words ( -- ) get-order 0 ?do ['] print-name swap traverse-wordlist loop ;

.( ... end of d-tools.f ) cr