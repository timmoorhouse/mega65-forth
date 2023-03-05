
: \ ( "ccc<eol>" -- ) 13 parse 2drop ; immediate ( k-return is not available yet )

\ The following words are implemented internally:                             
\
\ 0<> 0> 2>R 2R> 2R@ :NONAME <> ?DO DEFER DEFER FALSE NIP PARSE PARSE-NAME    
\ PICK REFILL RESTORE-INPUT ROLL S\" SAVE-INPUT SOURCE-ID TRUE UNUSED VALUE   

: /string ( c-addr1 u1 n -- c-addr2 u2 ) 
  dup >r - swap r> + swap ; ( STRING )

\ *************************************************************************** 

\ .( see core.f

\ .R see core.f

\ see also do in core.f
: ?do ( C: -- do-sys ) ( n1|u1 n2|u2 -- ) ( R: loop-sys ) 
  postpone (?do) 0 , here ( 3 ) ; immediate ( compile-only )

: action-of ( "<spaces>name" -- )
   state @ if
     postpone ['] postpone defer@
   else
     ' defer@
   then ; immediate
   
\ AGAIN see core.f

: buffer: ( u "<name>" -- ; -- addr ) create allot ;

\ TODO c"
: c" ( "ccc<quote>" -- ) ( -- c-addr ) 
  [char] " parse postpone csliteral ; immediate compile-only

\ TODO From discussion in ANSI A.3.2.3.2:
\ 0 CONSTANT CASE IMMEDIATE
: case ( C: -- case-sys ) ( -- ) 0 ; immediate compile-only

: compile, ( xt -- ) , ; ( compile-only )

\ DEFER@ see core.f

\ DEFER! see core.f

: endcase ( C: case-sys -- ) ( x -- )
  postpone drop 0 ?do postpone then loop ; immediate compile-only

: endof  ( C: case-sys1 of-sys -- case-sys2 ) ( -- )
  >r postpone else r> ; immediate compile-only

: erase ( addr u ) 0 fill ;

: hex ( -- ) #16 base ! ;

: holds ( c-addr u -- ) begin dup while 1- 2dup + c@ hold repeat 2drop ;

\ IS see core.f

\ TODO marker
: marker ( "<spaces>name" -- ) ( -- ) create does> ;

: of ( C: -- of-sys ) ( x1 x1 -- | x1 ) 
  1+ >r postpone over postpone = postpone if postpone drop r> ; immediate compile-only

\ PAD see core.f

: to ( x "<spaces>name" -- ) 
  state @ if
    postpone ['] postpone >body postpone !
  else
    ' >body ! 
  then ; immediate

: tuck ( x1 x2 -- x2 x1 x2 ) swap over ;

\ U.R see core.f

: u> ( u1 u2 -- flag ) swap u< ;

: within ( n1|u1 n2|u2 n3|u3 -- flag ) over - >r - r> u< ;

\ *************************************************************************** 

\ Add character C to the contents of address C-ADDR.
: c+! ( char c-addr -- ) tuck c@ + swap c! ;

\  Add the character to the end of the counted string.
: addchar ( char c-addr -- ) tuck count + c! 1 swap c+! ;

\ Add the string described by C-ADDR U to the counted string at
\ $DEST. The strings must not overlap.
: append ( c-addr u $dest -- )
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
       7 c, \ \a BEL (Alert)
     #20 c, \ \b BS  (Backspace)
  char c c, \ \c
  char d c, \ \d
     #27 c, \ \e ESC (Escape)
     #12 c, \ \f FF  (Form feed) \ TODO !!!!!!!!
  char g c, \ \g
  char h c, \ \h
  char i c, \ \i
  char j c, \ \j
  char k c, \ \k
     #10 c, \ \l LF  (Line feed)
  char m c, \ \m
     #13 c, \ \n
  char o c, \ \o
  char p c, \ \p
  char " c, \ \q "   (Double quote)
     #13 c, \ \r CR  (Carriage Return)
  char s c, \ \s
       9 c, \ \t HT  (horizontal tab}
  char u c, \ \u
     #11 c, \ \v VT  (vertical tab) \ TODO !!!!!!!!!
  char w c, \ \w
  char x c, \ \x
  char y c, \ \y
       0 c, \ \z NUL (no character)

\ Add an escape sequence to the counted string at dest,
\ returning the remaining string.
: addEscape ( c-addr len dest -- c-addr' len' )
  over 0= if drop exit then             \ zero length check
  >r                                    \ -- caddr len ; R: -- dest
  over c@ [char] x = if                 \ hex number?
    1 /string extract2H r> addchar exit
  then
  over c@ [char] m = if                 \ CR/LF pair
    1 /string  #13 r@ addchar #10 r> addchar exit
  then
  over c@ [char] a [char] z 1+ within if
    over c@ [char] a - EscapeTable + c@ r> addchar
  else
    over c@ r> addchar
  then
  1 /string
  ;

\ Parses a string up to an unescaped '"', translating '\'
\ escapes to characters. The translated string is a
\ counted string at *\i{dest}.
\ The supported escapes (case sensitive) are:
\ \a      BEL          (alert)
\ \b      BS           (backspace)
\ \e      ESC (not in C99)
\ \f      FF           (form feed)
\ \l      LF (ASCII 10)
\ \m      CR/LF pair - for HTML etc.
\ \n      newline - CRLF for Windows/DOS, LF for Unices
\ \q      double-quote
\ \r      CR (ASCII 13)
\ \t      HT (tab)
\ \v      VT
\ \z      NUL (ASCII 0)
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
  r> drop
  ;

\ Parses an escaped string from the input stream according to
\ the rules of *\fo{parse\"} above, returning the address
\ of the translated counted string in *\fo{POCKET}.
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

.( ... end of core-ext.f ) cr