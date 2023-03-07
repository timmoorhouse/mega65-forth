
  s" d-core"          included
  s" d-core-ext"      included
  s" d-file"          included

:noname ; is autoboot \ TODO could use decimal to save a few bytes

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
\ . .S AUTOBOOT (QUIT)

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

  s" d-block"         included
  s" d-block-ext"     included
  s" d-double"        included
  s" d-double-ext"    included
  s" d-exception"     included
\ s" d-exception-ext" included    \ TODO no need for one yet
  s" d-facility"      included
  s" d-facility-ext"  included
  s" d-file-ext"      included
  s" d-floating"      included
  s" d-floating-ext"  included
  s" d-locals"        included
  s" d-locals-ext"    included
  s" d-memory"        included
\ s" d-memory-ext"    included    \ TODO no need for one yet
  s" d-search"        included
  s" d-search-ext"    included
  s" d-string"        included
  s" d-string-ext"    included
  s" d-tools"         included
  s" d-tools-ext"     included
  s" d-xchar"         included
  s" d-xchar-ext"     included

:noname
  case
  3 of  4 foreground           endof \ error  - purple
  2 of 14 foreground ." ok" cr endof \ prompt - lt blue
  1 of  7 foreground           endof \ input  - yellow
  ( 0 ) 1 foreground                 \ output - white
  endcase ; is theme

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

.( ... bootstrap stage 2 complete ) cr cr

unused . s" bytes free" type cr \ 26865

: test s" test" included ;

: bm s" benchmark" included ;

: foo 5 abort" abc" ;

\ -1  CONSTANT EXC_ABORT
\ -2  CONSTANT EXC_ABORT"
\ -13 CONSTANT EXC_UNDEF
\ : T6 ABORT ;
\ 
\ : T10 77 SWAP ABORT" This should not be displayed" ;
\ 
\ : C6 CATCH
\    >R   R@ EXC_ABORT  = IF 11
\    ELSE R@ EXC_ABORT" = IF 12
\    ELSE R@ EXC_UNDEF  = IF 13
\    THEN THEN THEN R> DROP
\ ;
\ 
\ 4 5 ' T10 C6 .s cr

\ : T7 S" 333 $$QWEQWEQWERT$$ 334" EVALUATE 335 ;
\ : T8 S" 222 T7 223" EVALUATE 224 ;
\ : T9 S" 111 112 T8 113" EVALUATE 114 ;
\ 
\ 6 7 ' T9 C6 3 .s cr
\ \ T{ 6 7 ' T9 C6 3 -> 6 7 13 3 }T         \ Test unlinking of sources

1 2 3   asdjfklj   4 5 6

\ HEX
\ : rp1 >r >r >r >r rp@ 1+ @ rp@ cell+ 1+ @ 2r> 2r> 2drop 2drop ;
\ 1234 5678 $abc $def rp1 .s

.( end of bootstrap2.f ) cr
