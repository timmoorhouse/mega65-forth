
: \ ( "ccc<eol>" -- ) #13 parse 2drop ; immediate ( k-return is not available yet )

\ The following words are implemented internally:                             
\ 0<> 0> 2>R 2R> 2R@ <> ?DO DEFER NIP PARSE PARSE-NAME    
\ PICK REFILL RESTORE-INPUT ROLL S\" SAVE-INPUT SOURCE-ID TRUE UNUSED VALUE   

\ The following words are implemented in bootstrap1.f:
\ .( :NONAME AGAIN TO

\ The following words are implemented in core.f:
\ .R DEFER@ DEFER! IS PAD U.R

: /string ( c-addr1 u1 n -- c-addr2 u2 ) 
  dup >r - swap r> + swap ; ( STRING )

\ *************************************************************************** 

\ see also do in core.f
: ?do ( C: -- do-sys ) ( n1|u1 n2|u2 -- ) ( R: loop-sys ) 
  postpone (?do) 0 , here ( 3 ) ; immediate ( compile-only )

: action-of ( "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer@
   else
     ' defer@
   then ; immediate
   
: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ TODO c"
: c" ( "ccc<quote>" -- ) ( -- c-addr ) 
  [char] " parse postpone csliteral ; immediate compile-only

\ TODO From discussion in ANSI A.3.2.3.2:
\ 0 CONSTANT CASE IMMEDIATE
: case ( C: -- case-sys ) ( -- ) 0 ; immediate compile-only

: compile, ( xt -- ) , ; ( compile-only )

: endcase ( C: case-sys -- ) ( x -- )
  postpone drop 0 ?do postpone then loop ; immediate compile-only

: endof  ( C: case-sys1 of-sys -- case-sys2 ) ( -- )
  >r postpone else r> ; immediate compile-only

: erase ( addr u ) 0 fill ;

0 constant false

: hex ( -- ) #16 base ! ;

: holds ( c-addr u -- ) begin dup while 1- 2dup + c@ hold repeat 2drop ;

: of ( C: -- of-sys ) ( x1 x1 -- | x1 ) 
  1+ >r postpone over postpone = postpone if postpone drop r> ; immediate compile-only

: tuck ( x1 x2 -- x2 x1 x2 ) swap over ;

: u> ( u1 u2 -- flag ) swap u< ;

: within ( n1|u1 n2|u2 n3|u3 -- flag ) over - >r - r> u< ;

\ *************************************************************************** 

\ Add character C to the contents of address C-ADDR.
: c+! ( char c-addr -- ) tuck c@ + swap c! ;

\  Add the character to the end of the counted string.
: addchar ( char c-addr -- ) tuck count + c! 1 swap c+! ;

\ Add the string described by c-addr1 u to the counted string at c-addr2.
\ The strings must not overlap.
: append ( c-addr1 u c-addr2 -- )
  >r
  tuck  r@ count +  swap cmove          \ add source to end
  r> c+!                                \ add length to count
  ;

\ Extract a two-digit hex number in the given base from the
\ start of the string, returning the remaining string
\ and the converted number.
: extract2H ( c-addr len -- c-addr' len' u )
  base @ >r  hex
  0 0 2over drop 2 >number 2drop drop
  >r  2 /string  r>
  r> base ! ;

\ Table of translations for \a..\z
create EscapeTable ( -- addr )
        7 c, \ \a BEL
      #20 c, \ \b BS      Differs from ASCII
( c ) #67 c, \ \c
( d ) #68 c, \ \d
      #27 c, \ \e ESC
     #147 c, \ \f FF      Differs from ASCII
( g ) #71 c, \ \g
( h ) #72 c, \ \h
( i ) #73 c, \ \i
( j ) #74 c, \ \j
( k ) #75 c, \ \k
      #10 c, \ \l LF
      #13 c, \ \m CR-LF   Handled separately
      #13 c, \ \n         CR on the MEGA65
( o ) #79 c, \ \o
( p ) #80 c, \ \p
   char " c, \ \q "
      #13 c, \ \r CR 
( s ) #83 c, \ \s
        9 c, \ \t HT
( u ) #85 c, \ \u
      #11 c, \ \v VT      TODO !!!!!!!!!
( w ) #87 c, \ \w
( x ) #88 c, \ \x         Handled separately
( y ) #89 c, \ \y
        0 c, \ \z NUL

\ Add an escape sequence to the counted string at dest,
\ returning the remaining string.
: addEscape ( c-addr len dest -- c-addr' len' )
  over 0= if drop exit then               \ zero length check
  >r                                      \ ( -- caddr len ) ( R: -- dest )
  over c@ #88 ( x ) = if                  \ hex number?
    1 /string extract2H r> addchar exit
  then
  over c@ #77 ( m ) = if                  \ CR/LF pair
    1 /string  #13 r@ addchar #10 r> addchar exit
  then
  \ TODO handle escape sequences using upper case?
  over c@ #65 #90 1+ within if
    over c@ #65 - EscapeTable + c@ r> addchar
  else
    over c@ r> addchar
  then
  1 /string                               \ default case
  ;

\ Parses a string up to an unescaped '"', translating '\'
\ escapes to characters. The translated string is a
\ counted string at *\i{dest}.
\ The supported escapes (case sensitive) are:
\ \a      BEL          (alert)
\ \b      BS
\ \e      ESC
\ \f      FF
\ \l      LF
\ \m      CR/LF pair
\ \n      newline - CR on the MEGA65
\ \q      double-quote
\ \r      CR
\ \t      HT
\ \v      VT
\ \z      NUL
\ \"      double-quote
\ \xAB    Two char Hex numerical character value
\ \\      backslash itself
\ \       before any other character represents that character
: parse\" ( c-addr len dest -- c-addr' len' )
  dup >r  0 swap c!                     \ zero destination
  begin                                 \ -- caddr len ; R: -- dest
    dup
   while
    over c@ [char] " <>                 \ check for terminator
   while
    over c@ [char] \ = if               \ deal with escapes
      1 /string r@ addEscape
    else                                \ normal character
      over c@ r@ addchar  1 /string
    then
  repeat then
  dup                                   \ step over terminating "
  if 1 /string  then
  r> drop ;

\ Parses an escaped string from the input stream according to
\ the rules of parse\" above, returning the address
\ of the translated counted string.
: readEscaped ( "ccc" -- c-addr )
  source >in @ /string tuck             \ -- len caddr len
  sbuf parse\" nip
  - >in +!
  sbuf ; \ TODO can't call sbuf twice

\ TODO s\"
\ see http://www.forth200x.org/escaped-strings.html
: s\" ( "ccc<quote>" -- ) ( -- c-addr u ) 
  readEscaped count
  state @ if postpone sliteral then
  ; immediate

.( ... end of d-core-ext.f ) cr