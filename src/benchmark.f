
\ see also https://theultimatebenchmark.org/

\
\ An intentionally naive fibonacci
\

: fib ( n1 -- n2 ) dup 1 > if 1- dup 1- recurse swap recurse + then ;

\ TODO ud.r?
: du.r >r <# #s #> r> over - spaces type ;

\ TODO ud.?
: du. 0 du.r space ;

: benchmark timer 2>r execute timer 2r> d- ;

\ desktop   4 468 645
\ laptop    4 468 742
\ MEGA65    5 644 686

: bm1 25 ['] fib benchmark du. . cr ;
