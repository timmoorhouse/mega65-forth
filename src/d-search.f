
\ The following words are implemented internally:
\ FORTH-WORDLIST WORDLIST

: get-current ( -- wid ) current @ ;

: set-current ( wid -- ) current ! ;

internals-wordlist set-current

variable #order

create context 8 ( wordlists ) cells allot

forth-wordlist set-current
 
 \ TODO what's supposed to happen when the search order is empty (n=0)?
: get-order ( -- widn ... wid1 n )
  #order @ 0 ?do
    #order @ i - 1- cells context + @
  loop
  #order @ ;

: set-order ( widn ... wid1 n -- )
  dup -1 = if
    drop forth-wordlist 1 \ <push system default word lists and n>
  then
  \ TODO limit length, throw E_SEARCH_ORDER_OVERFLOW
  dup #order !
  0 ?do i cells context + ! loop
  ;

\ -1 set-order
forth-wordlist internals-wordlist 2 set-order

: search-wordlist ( c-addr u wid -- 0 | xt 1 | xt -1 )
  find-name-in dup if
    dup name>interpret swap ?immediate if 1 else true then ( TODO name>xt? )
  then ;

:noname ( c-addr u -- nt | 0 )
  2>r get-order 2r> rot 0 swap 0 ?do ( widn ... widi c-addr u nt|0 )
    ?dup 0= if
      ( widn ... widi c-addr u )
      2>r 2r@ rot 2r> rot find-name-in ( TODO KLUNKY !!!!)
    else
      3 roll drop
    then
  loop nip nip ; is find-name

: definitions ( -- )
  \ TODO what if order is empty?
  get-order swap set-current 1- 0 ?do drop loop ;

.( ... end of d-search.f ) cr