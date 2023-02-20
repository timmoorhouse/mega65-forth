

\ : erase ( c-addr u ) 0 fill ;

: hex 16 base ! ;

: pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr