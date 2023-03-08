
: also ( -- ) get-order over swap 1+ set-order ;

: (wordlist) ( wid "<spaces>name" -- )
   create ,
   does> @ >r get-order nip r> swap set-order ;
forth-wordlist (wordlist) forth

: only ( -- ) -1 set-order ;

\ TODO order

\ TODO E_SEARCH_ORDER_UNDERFLOW
: previous ( -- ) get-order nip 1- set-order ;

.( ... end of d-search-ext.f ) cr