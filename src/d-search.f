
\ The following words are implemented internally:
\ FORTH-WORDLIST SEARCH-WORDLIST WORDLIST

: get-current ( -- wid ) current @ ;

: set-current ( wid -- ) current ! ;

variable #order

create context 8 ( wordlists ) cells allot
 
 \ TODO what's supposed to happen when the search order is empty (n=0)?
: get-order ( -- widn ... wid1 n )
  #order @ 0 ?do
    #order @ i - 1- cells context + @
  loop
  #order @ ;

: set-order ( widn ... wid1 n -- )
  dup -1 = if
    \ drop <push system default word lists and n>
  then
  \ TODO limit length, throw E_SEARCH_ORDER_OVERFLOW
  \ dup #order !
  \ 0 ?do i cells context + ! loop
  ;

: definitions ( -- )
  \ TODO what if order is empty?
  get-order swap set-current 0 ?do drop loop ;

.( ... end of d-search.f ) cr