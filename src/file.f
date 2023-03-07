
\ The following words are implemented internally:
\
\ BIN CLOSE-FILE CREATE-FILE DELETE-FILE FILE-POSITION FILE-SIZE INCLUDE-FILE
\ OPEN-FILE READ-FILE READ-LINE REPOSITION-FILE RESIZE-FILE
\ WRITE-FILE WRITE-LINE

1 constant r/o
2 constant w/o
3 constant r/w

: bin ( fam1 -- fam2 ) 4 or ;

: included ( c-addr u ) r/o open-file if -38 throw then 
  >r r@ include-file r> close-file drop ;

\ : write-file ;

.( ... END OF FILE.F ) cr