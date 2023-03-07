
\ The following words are implemented internally:
\ BIN CLOSE-FILE CREATE-FILE DELETE-FILE FILE-POSITION FILE-SIZE INCLUDE-FILE
\ OPEN-FILE READ-FILE READ-LINE REPOSITION-FILE RESIZE-FILE
\ WRITE-FILE WRITE-LINE

\ The following words are implemented in bootstrap1.f:
\ INCLUDED R/O

2 constant w/o
3 constant r/w

: bin ( fam1 -- fam2 ) 4 or ;

\ : write-file ;

.( ... end of file.f ) cr