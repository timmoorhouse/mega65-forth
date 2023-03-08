
: get-current ( -- wid ) current @ ;

: set-current ( wid -- ) current ! ;

\ variable #order
\ 
\ create context 16 ( wordlists ) cells allot
 
: get-order ( -- widn ... wid1 n )
  \ #order @ 0 ?do
  \   #order @ i - 1- cells context + @
  \ loop
  \ #order @
  ;

: set-order ( widn ... wid1 n -- )
  dup -1 = if
    \ drop <push system default word lists and n>
  then
  \ dup #order !
  \ 0 ?do i cells context + ! loop
  ;

: definitions ( -- )
  get-order swap set-current 0 ?do drop loop ;

.( ... end of d-search.f ) cr