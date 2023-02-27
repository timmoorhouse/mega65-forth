
\ see also https://theultimatebenchmark.org/

\
\ An intentionally naive fibonacci
\

: fib ( n1 -- n2 ) dup 1 > if 1- dup 1- recurse swap recurse + then ;

20 fib .
