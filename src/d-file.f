
\ The following words are implemented internally:
\ OPEN-FILE READ-LINE RESIZE-FILE

\ The following words are implemented in bootstrap1.f:
\ CLOSE-FILE INCLUDE-FILE INCLUDED R/O

2 constant w/o
3 constant r/w

: bin ( fam1 -- fam2 ) 4 or ;

\ TODO create-file ( c-addr u fam -- fileid ior )

\ TODO delete-file ( c-addr u -- ior )

\ TODO file-position ( fileid -- ud ior )

\ TODO file-size ( fileid -- ud ior )

\ TODO read-file ( c-addr u1 fileid -- u2 ior )

\ TODO reposition-file ( ud fileid -- ior )

\ TODO resize-file ( ud fileid -- ior )

: write-file ( c-addr u fileid -- ior ) 
  k-chkout over + swap ?do i c@ emit loop 0 k-chkout k-readss ;

\ TODO duplication with write-file
: write-line ( c-addr u fileid -- ior )
  k-chkout over + swap ?do i c@ emit loop cr 0 k-chkout k-readss ;

.( ... end of d-file.f ) cr