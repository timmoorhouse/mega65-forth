
: compile, , ;

: erase ( c-addr u ) 0 fill ;

: hex 16 base ! ;

: is ( xt "<spaces>name" -- ) 
    parse-name forth-wordlist search-wordlist if >body ! then ; immediate \ TODO error if not found

: tuck swap over ;

: u> swap u< ;

\ : pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr