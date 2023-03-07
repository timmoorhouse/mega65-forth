
\ The following words are implemented internally:
\ CMOVE CMOVE> COMPARE

\ The following words are implemented in bootstrap1.f:
\ SLITERAL

\ ***************************************************************************

\ /STRING see core-ext.f

: blank ( c-addr u ) bl fill ;

\ Replace each '%' character in the input string c-addr1 len1 with two '%' characters.
\ The output is represented by c-addr2 len2.
\ If you pass a string through UNESCAPE and then SUBSTITUTE, you get the original string.
: unescape ( c-addr1 len1 c-addr2 -- c-addr2 len2 )
   dup 2swap over + swap ?do
     i c@ [char] % = if
       [char] % over c! 1+
     then
     i c@ over c! 1+
   loop
   over -
;

.( ... end of string.f ) cr