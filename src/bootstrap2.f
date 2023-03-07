
  s" core.f"          included
  s" core-ext.f"      included
  s" file.f"          included

:noname ; is autoboot \ TODO could use decimal to save a few bytes

: savesystem ( "<spaces>name" -- ) parse-name w/o open-file drop \ TODO check status from open-file
  >r ( R: fid )
  sp@ 2 r@ write-file drop \ TODO check status from write-file
  $2001 dup here swap - r@ write-file drop \ TODO check status
  r> close-file drop \ TODO check status
  ;

.( ... saving forth-minimal ) cr
savesystem forth-minimal,p,w

  s" block.f"         included
  s" block-ext.f"     included
  s" double.f"        included
  s" double-ext.f"    included
  s" exception.f"     included
\ s" exception-ext.f" included    \ TODO no need for one yet
  s" facility.f"      included
  s" facility-ext.f"  included
  s" file-ext.f"      included
  s" floating.f"      included
  s" floating-ext.f"  included
  s" locals.f"        included
  s" locals-ext.f"    included
  s" memory.f"        included
\ s" memory-ext.f"    included    \ TODO no need for one yet
  s" search.f"        included
  s" search-ext.f"    included
  s" string.f"        included
  s" string-ext.f"    included
  s" tools.f"         included
  s" tools-ext.f"     included
  s" xchar.f"         included
  s" xchar-ext.f"     included

:noname
  case
  3 of  4 foreground endof \ error  - purple
  2 of 14 foreground endof \ prompt - lt blue
  1 of  7 foreground endof \ input  - yellow
  ( 0 ) 1 foreground       \ output - white
  endcase ; is theme

.( ... saving forth-complete ) cr
savesystem forth-complete,p,w

.( ... Bootstrap stage 2 complete ) cr

unused . s" bytes free" type cr \ 26920

: test s" runtests.f" included ;

: bm s" benchmark.f" included ;

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
