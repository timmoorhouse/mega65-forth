
\ TODO eventually get rid of the defer ...
\ TODO if base is 10 used signed output, otherwise unsigned?
: (.s) '<' emit depth 0 u.r '>' emit space depth if 0 depth 2- do i pick . -1 +loop then ;

' (.s) is .s

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
    5 dump
    r> base ! ;

: see ( "<spaces>name" -- )
    base @ >r hex cr
    parse-name forth-wordlist search-wordlist-nt ?dup if
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

.( end of tools.f )