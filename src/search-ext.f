
: also ( -- ) get-order over swap 1+ set-order ;

\ : (wordlist) ( wid "<name>" -- ; )
\    create ,
\    does>
\      @ >r
\      get-order nip
\      r> swap set-order
\ ;
\ forth-wordlist (wordlist) forth

\ : only ( -- ) -1 set-order ;

\ : previous ( -- ) get-order nip 1- set-order ;

.( ... end of search-ext.f ) cr