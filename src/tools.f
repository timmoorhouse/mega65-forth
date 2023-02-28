
\ TODO if base is 10 used signed output, otherwise unsigned?
: .s '<' emit depth 0 u.r '>' emit space depth if 0 depth 2- do i pick . -1 +loop then ;

: ? @ . ;

: dump ( addr u -- ) \ u is number of lines to display
    base @ >r hex cr
    0 do   ( addr )
        16 ( addr u2 )
        over 4 u.r 
        2dup over + swap do space i c@ 2 u.r loop
        \ space ':' emit space
        \ 2dup over + swap do i c@ emit loop \ TODO would need a check for printable chars
        cr
        +
    loop
    r> base ! ;

\ following gforth ...
: xt-see ( xt -- ) 
    base @ >r hex
    \ dup 4 u.r space ." TODO - xt-see "
    3 dump
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

: id. ( nt -- ) name>string type ;

\ TODO use a :noname ...
: print-name ( nt -- u ) 
    out c@ 
    dup if space then
    c/l > if cr then
    id.
    true ;

: words ( -- ) ['] print-name forth-wordlist traverse-wordlist ;

.( ... end of tools.f ) cr