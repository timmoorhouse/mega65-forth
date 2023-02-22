
: 0> swap 0< ;
: compile, , ;

\ : erase ( c-addr u ) 0 fill ;

: hex 16 base ! ;

: u> swap u< ;

\ : pad here 68 + ; \ TODO FIG uses some space in the gap for WORDS ... clean this up

.( ... end of core-ext.f ) cr