
\ The following words are implemented internally:                             

\ 2CONSTANT D+ DNEGATE 

environment-wordlist current !

internals-wordlist current !

forth-wordlist current !

\ *************************************************************************** 

\ TODO 2constant - move to forth from internal

: 2literal ( x1 x2 -- ) ( -- x1 x2 ) 
  postpone (2literal) swap , , ; immediate compile-only

\ TODO 2VARIABLE

\ TODO D-

\ D. see core.f

\ D.R see core.f

: d0< ( d -- flag ) 0< nip ;

: d0= ( xd -- flag ) or 0= ;

\ TODO D2*

\ TODO D2/

\ TODO D<

: d= ( xd1 xd2 -- flag ) xor 0= ;

: d>s ( d -- n ) drop ;

\ DABS see core.f

\ TODO DMAX

\ TODO DMIN

\ TODO M*

\ TODO M+

.( ... end of d-double.f ) cr