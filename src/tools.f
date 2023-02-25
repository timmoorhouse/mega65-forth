
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

: see ( "<spaces>name" -- ) ;

\ TODO traverse-namelist should do the hidden check
\ : print-name ( nt -- u ) dup ?hidden =0 if
\         out @ 
\             dup if space then
\             c/l greater if cr then
\         name>string type
\     then 
\     true
\ ;
\ 
\ : words ( -- ) ' print-name forth-wordlist traverse-wordlist ;

.( end of tools.f )