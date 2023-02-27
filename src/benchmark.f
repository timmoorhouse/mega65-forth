
\ see also https://theultimatebenchmark.org/

\
\ An intentionally naive fibonacci
\

: fib ( n1 -- n2 ) dup 1 > if 1- dup 1- recurse swap recurse + then ;

: du.r >r <# #s #> r> over - spaces type ;
: du. 0 du.r space ;

: benchmark timer 2>r execute timer 2r> d- du. ; \ TODO d-

\ desktop         TBD
\ laptop    4 483 749
\ MEGA65    5 662 489

: bm1 25 ['] fib benchmark . cr ;
