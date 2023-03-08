
: also ( -- ) get-order over swap 1+ set-order ;

: (wordlist) ( wid "<spaces>name" -- )
   create ,
   does> @ >r get-order nip r> swap set-order ;
forth-wordlist (wordlist) forth

: only ( -- ) -1 set-order ;

\ Pretty simple display format ... it gets the job done though
: order ( -- ) '[' emit space get-order 0 ?do . loop ']' emit space get-current . ;

\ TODO E_SEARCH_ORDER_UNDERFLOW
: previous ( -- ) get-order nip 1- set-order ;

.( ... end of d-search-ext.f ) cr