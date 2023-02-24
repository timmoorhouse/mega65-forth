
: dump ( addr u -- )

    2dup 16 min \ ( addr u addr u2 )
    over . 

    \ 2dup over + swap do i c@ . loop

    space
    char | emit
    space

    \ 2dup over + swap do i c@ emit loop

    cr
    2drop

    2drop ;

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