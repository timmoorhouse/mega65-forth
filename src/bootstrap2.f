
cr .( Starting bootstrap stage 2... ) cr

  s" d-core.f"          included
  s" d-core-ext.f"      included
  s" d-file.f"          included
  s" d-search.f"        included

' decimal is autoboot

: savesystem ( "<spaces>name" -- ) parse-name w/o open-file drop \ TODO check status from open-file
  >r ( R: fid )
  sp@ 2 r@ write-file drop \ TODO check status from write-file
  $2001 dup here swap - r@ write-file drop \ TODO check status
  r> close-file drop \ TODO check status
  ;
  
:noname ( -- ) postpone [ 0 to source-id
  begin
    refill drop \ TODO check result from REFILL
    (evaluate)
    state @ 0= if 
      2 theme \ prompt
    then 
  again ; is (quit)

\ Before saving a system, the following deferred words MUST be defined:
\ . .S AUTOBOOT (QUIT) FIND-NAME

forth-wordlist 1 set-order

.( ... saving forth-minimal )
savesystem forth-minimal,p,w
unused . s" bytes free" type cr \ 28532

environment-wordlist forth-wordlist internals-wordlist 3 set-order

  s" d-block.f"         included
  s" d-block-ext.f"     included
  s" d-double.f"        included
  s" d-double-ext.f"    included
  s" d-exception.f"     included
\ s" d-exception-ext.f" included    \ TODO no need for one yet
  s" d-facility.f"      included
  s" d-facility-ext.f"  included
  s" d-file-ext.f"      included
  s" d-floating.f"      included
  s" d-floating-ext.f"  included
  s" d-locals.f"        included
  s" d-locals-ext.f"    included
  s" d-memory.f"        included
\ s" d-memory-ext.f"    included    \ TODO no need for one yet
  s" d-search-ext.f"    included
  s" d-string.f"        included
  s" d-string-ext.f"    included
  s" d-tools.f"         included
  s" d-tools-ext.f"     included
  s" d-xchar.f"         included
  s" d-xchar-ext.f"     included

:noname
  case
  3 of  4 foreground           endof \ error  - purple
  2 of 14 foreground ." ok" cr endof \ prompt - lt blue
  1 of  7 foreground           endof \ input  - yellow
  ( 0 ) 1 foreground                 \ output - white
  endcase ; is theme

: marker ( "<spaces>name" -- ) ( -- ) 
  here 
  create 
    ,                               \ save here
    get-current ,                   \ save current
    get-order dup , 0 ?do , loop    \ save order
    \ save fixup to account for head of current wordlist changing when
    \ we did the create
    get-current @ @ get-current forth-wordlist - here +
    forth-wordlist here 20 dup allot cmove \ save wordlist table
    ! \ do the fixup
  does> 
    dup @ to here cell+             \ restore here
    dup @ set-current cell+           \ restore current
    dup dup @ 0 ?do dup dup @ i - cells + @ swap loop @ set-order dup @ 1+ cells + \ restore order
    forth-wordlist 20 cmove \ restore wordlist table
  ;

: type-file ( fileid -- )
  cr >r r@ fileid>buffer 
  begin 
    ( c-addr u ) ( R: fileid )
    2dup r@ read-line throw ( c-addr u u2 flag ior ) ( R: fileid )
  while
    ( c-addr u u2 )
    2 pick swap type cr
  repeat 2drop r> drop ;

: type ( "<spaces>name" ) parse-name r/o open-file 0<> -38 and throw
  dup type-file close-file drop ;

only forth definitions

.( ... saving forth-complete )
savesystem forth-complete,p,w
unused . s" bytes free" type cr \ 26141

.( ... bootstrap stage 2 complete ) cr cr

: test s" test.f" included ;

: bm s" benchmark.f" included ;

1 2 3   asdjfklj   4 5 6

\ .( pre marker ) cr
\ .( here= ) here . cr
\ 
\ marker foo
\ 
\ 100 allot
\ 
\ cr
\ .( after creating marker ) cr
\ .s cr
\ .( here= ) here . cr
\ .( foo= ) s" foo" find-name . cr
\ 
\ foo
\ 
\ cr
\ .( after running marker ) cr
\ .s cr
\ .( here= ) here . cr
\ .( foo= ) s" foo" find-name . cr

.( end of bootstrap2.f ) cr
