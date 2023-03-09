
\ The following words are implemented internally:
\ READ-LINE

\ The following words are implemented in bootstrap1.f:
\ CLOSE-FILE INCLUDE-FILE INCLUDED OPEN-FILE R/O

2 constant w/o
3 constant r/w

: bin ( fam1 -- fam2 ) 4 or ;

\ TODO create-file ( c-addr u fam -- fileid ior )

\ TODO delete-file ( c-addr u -- ior )

\ TODO file-position ( fileid -- ud ior )

\ TODO file-size ( fileid -- ud ior )

\ TODO read-file ( c-addr u1 fileid -- u2 ior )

\ : read-line ( c-addr u1 fileid -- u2 flag ior )
\   k-chkin over + over ?do 
\     k-basin dup #13 = if 1 source-line +! drop leave else i c@ 1+ then
\   +loop true 0 k-chkin k-readss ;

\ TODO reposition-file ( ud fileid -- ior )

\ TODO resize-file ( ud fileid -- ior )

: write-file ( c-addr u fileid -- ior ) 
  k-chkout over + swap ?do i c@ emit loop 0 k-chkout k-readss ;

\ TODO duplication with write-file
: write-line ( c-addr u fileid -- ior )
  k-chkout over + swap ?do i c@ emit loop cr 0 k-chkout k-readss ;

.( ... end of d-file.f ) cr