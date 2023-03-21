
\ The following words are implemented internally:                             
\ 2CONSTANT D+ D- DNEGATE 

\ The following words are implemented in core.f:
\ D. D.R DABS

environment-wordlist current !

internals-wordlist current !

forth-wordlist current !

\ *************************************************************************** 

\ TODO 2constant - move to forth from internal

: 2literal ( x1 x2 -- ) ( -- x1 x2 ) 
  postpone (2literal) swap , , ; immediate compile-only

\ TODO 2VARIABLE

: d0< ( d -- flag ) 0< nip ;

: d0= ( xd -- flag ) or 0= ;

\ TODO D2*

\ TODO D2/

: d< ( xd1 xd2 -- flag ) d- d0< ;

: d= ( xd1 xd2 -- flag ) d- d0= ;

: d>s ( d -- n ) drop ;

\ TODO DMAX

\ TODO DMIN

\ TODO M*

\ TODO M+

.( ... end of d-double.f ) cr