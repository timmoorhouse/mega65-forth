
\ : ALSO ( -- ) GET-ORDER OVER SWAP 1+ SET-ORDER ;

\ : (wordlist) ( wid "<name>" -- ; )
\    CREATE ,
\    DOES>
\      @ >R
\      GET-ORDER NIP
\      R> SWAP SET-ORDER
\ ;
\ FORTH-WORDLIST (wordlist) FORTH

\ : ONLY ( -- ) -1 SET-ORDER ;

\ : PREVIOUS ( -- ) GET-ORDER NIP 1- SET-ORDER ;

.( ... end of search-ext.f ) cr