
; FIG-specific stuff

!if ENABLE_FIG {

; ****************************************************************************
; ->
;      -->                                                   P,L0
;               Continue interpretation with the next disc screen.  
;               (pronounced next-screen).

;;
;;                                       -->
;;                                       SCREEN 62 LINE 6
;;

!if 0 {
        +WORD "-->"
W_DASHDASH
        !word DO_COLON
;          !word QLOAD
;          !word ZERO
;          !word IN
;          !word STORE
;          !word BSCR
;          !word BLK
;          !word AT
;          !word OVER
;          !word MOD
;          !word SUB
;          !word BLK
;          !word PSTOR
        !word W_PSEMI
}

; ****************************************************************************
; -BCD

;;
;;                                       -BCD
;;                             Convert binary value to BCD
;;

!if 0 {
        +WORD "-bcd"
W_DBCD
        !word DO_COLON
;          !word ZERO,CLIT
;          !byte 10
;          !word USLAS,CLIT
;          !byte 16
;          !word STAR,OR
        !word W_PSEMI
}

; ****************************************************************************
; B/BUF

;      B/BUF         ---  n
;               This constant leaves the number of bytes per disc buffer, 
;               the byte count read from disc by BLOCK.

;;
;;                                       B/BUF
;;                                       SCREEN 35 LINE 9
;;                                       Bytes per Buffer
;;

!if 0 {
        +WORD "b/buf"
W_BBUF
        !word DO_CONSTANT
;          !word SSIZE    ; sector size
}

; ****************************************************************************
; B/SCR

;      B/SCR         ---  n
;               This constant leaves the number of blocks per editing 
;               screen.  By convention, an editing screen is 1024 bytes 
;               organised as 16 lines of 64 characters each.

;;
;;                                       B/SCR
;;                                       SCREEN 35 LINE 10
;;                                       Blocks per screen
;;

!if 0 {
        +WORD "b/scr"
W_BSCR
        !word DO_CONSTANT
;          !word 8        ; blocks to make one screen
;
}

; ****************************************************************************
; +BUF

;      +BUF          addr1  ---  addr2  f
;               Advance the disc buffer address addr1 to the address of 
;               the next buffer addr2.  Boolean f is false when addr2 is 
;               the buffer presently pointed to by variable PREV.


;;
;;                                       +BUF
;;                                       SCREEN 58 LINE 4
;;
;;

!if 0 {
        +WORD "+buf"
W_PBUF
        !word DO_COLON
;          !word LIT
;          !word SSIZE+4  ; hold block #, one sector two num
;          !word PLUS
;          !word DUP
;          !word LIMIT
;          !word EQUAL
;          !word ZBRAN
;L2688:    !word 6        ; L2691-L2688
;          !word DROP
;          !word FIRST
;L2691:    !word DUP
;          !word PREV
;          !word AT
;          !word SUB
        !word W_PSEMI
}

; ****************************************************************************
; <BUILDS

;      <BUILDS                                               C,L0
;               Used within a colon-definition:
;                         : cccc  <BUILDS  ...  DOES>  ...  ;
;               Each time cccc is executed, <BUILDS defines a new word 
;               with a high-level execution procedure.  Executing cccc in 
;               the form:
;                         cccc  nnnn
;               uses <BUILDS to create a dictionary entry for nnnn with a 
;               call to the DOES> part for nnnn.  When nnnn is later 
;               executed, it has the address of its parameter area on the 
;               stack and executes the words after DOES> in cccc.  <BUILDS 
;               and DOES> allow run-time procedures to be written in high-
;               level rather than in assembler code (as required by 
;               ;CODE ).


;;
;;                                       <BUILDS
;;                                       SCREEN 43 LINE 2
;;

!if 0 {
        +WORD "<builds"
W_BUILD
        !word DO_COLON
;          !word ZERO
;          !word W_CONSTANT
        !word W_PSEMI
}

; ****************************************************************************
; C/L

;;
;;                                       C/L
;;                                       SCREEN 35 LINE 5
;;                                       Characters per line

        +WORD "c/l"
W_CSLL
        !word DO_CONSTANT
        !word 64

; ****************************************************************************
; CONTEXT

;      CONTEXT       ---  addr                               U,L0
;               A user variable containing a pointer to the vocabulary 
;               within which dictionary searches will first begin.

;;
;;                                       CONTEXT
;;                                       SCREEN 37 LINE 2
;;

!if 0 {
        +WORD "context"
W_CONTEXT
        !word DO_USER
;          !byte $20
}

; ****************************************************************************
; DLITERAL

;      DLITERAL      d  ---  d           (executing)
;                    d  ---              (compiling)         P
;               If compiling, compile a stack double number into a 
;               literal.  Later execution of the definition containing 
;               the literal will push it to the stack.  If executing, the 
;               number will remain on the stack.

;;
;;                                       DLITERAL
;;                                       SCREEN 51 LINE 8
;;

!if 0 {
        +WORD "dliteral"
W_DLITERAL
        !word DO_COLON
;          !word STATE
;          !word AT
;          !word ZBRAN
;L2238:    !word 8        ; L2242-L2238
;          !word SWAP
;          !word LITER
;          !word LITER
        !word W_PSEMI
}

; ****************************************************************************
; -DISC

;;
;;                                       -DISC
;;                                       machine level sector R/W
;;

!if 0 {
        +WORD "-disc"
W_DDISC
        !word *+2
;          LDA 0,X
;          STA $C60C
;          STA $C60D      ; store sector number
;          LDA 2,X
;          STA $C60A
;          STA $C60B      ; store track number
;          LDA 4,X
;          STA $C4CD
;          STA $C4CE      ; store drive number
;          STX XSAVE
;          LDA $C4DA      ; sense read or write
;          BNE L3032
;          JSR $E1FE
;          JMP L3040
;L3032:    JSR $E262
;L3040:    JSR $E3EF      ; head up motor off
;          LDX XSAVE
;          LDA $C4E1      ; report error code
;          STA 4,X
        jmp POPTWO
}

; ****************************************************************************
; DPL

;      DPL           ---  addr                               U,L0
;               A user variable containing the number of digits to the 
;               right of the decimal on double integer input.  It may also 
;               be used to hold output column location of a decimal point, 
;               in user generated formatting.  The default value on single 
;               number input is -1.

!if 0 {
        +WORD "dpl"
W_DPL
        !word DO_USER
        !byte U_DPL
}

; ****************************************************************************
; FENCE

;      FENCE         ---  addr                               U
;               A user variable containing an address below which 
;               FORGETting is trapped.  To forget below this point the 
;               user must alter the contents of FENCE.

;;
;;                                       FENCE
;;                                       SCREEN 36 LINE 7
;;

!if 0 {
        +WORD "fence"
W_FENCE
        !word DO_USER
        !byte U_FENCE
}

; ****************************************************************************
; FIRST

;      FIRST         ---  n
;               A constant that leaves the address of the first (lowest) 
;               block buffer.

;;
;;                                       FIRST
;;                                       SCREEN 35 LINE 7

!if 0 {
        +WORD "first"
W_FIRST
        !word DO_CONSTANT
;          !word DAREA    ; bottom of disk buffer area
}

; ****************************************************************************
; FLD

;      FLD           ---  addr                               U
;               A user variable for control of number output field width.  
;               Presently unused in fig-FORTH.

;;
;;                                       FLD
;;                                       SCREEN 37 LINE 7

!if 0 {
        +WORD "fld"
W_FLD
        !word DO_USER
;          !byte $2A
}

; ****************************************************************************
; HLD

;      HLD           ---  addr                               L0
;               A user variable that holds the address of the latest 
;               character of text during numeric output conversion.

;;
;;                                       HLD
;;                                       SCREEN 37 LINE 10
;;

!if 0 {
        +WORD "hld"
W_HLD
        !word DO_USER
;          !byte $30
}

; ****************************************************************************
; INDEX

;      INDEX         from  to  ---
;               Print the first line of each screen over the range from, 
;               to.  This is used to view the comment lines of an area of 
;               text on disc screens.

;;
;;                                       INDEX
;;                                       SCREEN 77 LINE 7
;;

!if 0 {
        +WORD "index"
W_INDEX
        !word DO_COLON
;          !word CR
;          !word 1PLUS
;          !word SWAP
;          !word PDO
;L3647:    !word CR
;          !word I
;          !word THREE
;          !word DOTR
;          !word SPACE
;          !word ZERO
;          !word I
;          !word DLINE
;          !word QTERM
;          !word ZBRAN
;L3657:    !word 4        ; L3659-L3657
;          !word LEAVE
;L3659:    !word PLOOP
;L3660:    !word $FFE6    ; L3647-L3660
;          !word CLIT
;          !byte $0A      ; PT WAS HERE
;;          !byte $0C      ; form feed for printer
;          !word EMIT
        !word W_PSEMI
}

; ****************************************************************************
; LIMIT

;      LIMIT         ---  n
;               A constant leaving the address just above the highest 
;               memory available for a disc buffer.  Usually this is the 
;               highest system memory.

;;
;;                                       LIMIT
;;                                       SCREEN 35 LINE 8
;;

!if 0 {
        +WORD "limit"
W_LIMIT
        !word DO_CONSTANT
;          !word UAREA    ; buffers end at user area
}

; ****************************************************************************
; (LINE)


;      (LINE)        n1  n2  ---  addr  count
;               Convert the line number n1 and the screen n2 to the disc 
;               buffer address containing the data.  A count of 64 
;               indicates the full line text length.

;;
;;
;;                                       (LINE)
;;                                       SCREEN 61 LINE 2
;;

!if 0 {
        +WORD "(line)"
W_PLINE
        !word DO_COLON
;          !word TOR
;          !word CSLL
;          !word BBUF
;          !word SSMOD
;          !word RFROM
;          !word BSCR
;          !word STAR
;          !word PLUS
;          !word BLOCK
;          !word PLUS
;          !word CSLL
        !word W_PSEMI
}

; ****************************************************************************
; .LINE

;      .LINE         line  scr  ---
;               Print on the terminal device, a line of text from the disc 
;               by its line and screen number.  Trailing blanks are 
;               suppressed.

;;
;;                                       .LINE
;;                                       SCREEN 61 LINE 6
;;

!if 0 {
        +WORD ".line"
W_DLINE
        !word DO_COLON
;          !word PLINE
;          !word DTRAI
;          !word TYPE
        !word W_PSEMI
}

; ****************************************************************************
; ?LOADING

;      ?LOADING
;               Issue an error message if not loading.

;;
;;                                       ?LOADING
;;                                       SCREEN 40 LINE 14
;;

!if 0 {
        +WORD "?loading"
W_QLOAD
        !word DO_COLON
;          !word BLK
;          !word AT
;          !word ZEQU
;          !word CLIT
;          !byte $16
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; MESSAGE

;      MESSAGE       n  ---
;               Print on the selected output device the text of line n 
;               relative to screen 4 of drive 0.  n may be positive or 
;               negative.  MESSAGE may be used to print incidental text 
;               such as report headers.  IF WARNING is zero, the message 
;               will simply be printed as a number (disc-unavailable).

;;
;;                                       MESSAGE
;;                                       SCREEN 61 LINE 9
;;

!if 0 {
        +WORD "message"
W_MESSAGE
        !word DO_COLON
;          !word WARN
;          !word AT
;          !word ZBRAN
;L2874:    !word $1B      ; L2888-L2874
;          !word DDUP
;          !word ZBRAN
;L2877:    !word $11      ; L2886-L2877
;          !word CLIT
;          !byte 4
;          !word W_OFFSET
;          !word AT
;          !word BSCR
;          !word SLASH
;          !word SUB
;          !word DLINE
;L2886:    !word BRAN
;L2887:    !word 13       ; L2891-L2887
;L2888:    !word PDOTQ
;          !byte 6,"MSG # "
;          !word DOT
        !word W_PSEMI
}

; ****************************************************************************
; PREV

;      PREV          ---  addr
;               A variable containing the address of the disc buffer most 
;               recently referenced.  The UPDATE command marks this buffer 
;               to be later written to disc.

;;
;;                                       PREV
;;                                       SCREEN 58 LINE 2
;;

!if 0 {
        +WORD "prev"
W_PREV
        !word DO_VARIABLE
;          !word DAREA
}

; ****************************************************************************
; R#

;      R#            ---  addr                               U
;               A user variable which may contain the location of an 
;               editing cursor, or other file related function.
;
;;
;;                                       R#
;;                                       SCREEN 37  LINE 9
;;

!if 0 {
        +WORD "r#"
W_RNUM
        !word DO_USER
;          !byte $2E
}

; ****************************************************************************
; ?TERMINAL

;      ?TERMINAL     ---  f
;               Perform a test of the terminal keyboard for actuation of 
;               the break key.  A true flag indicates actuation.  This 
;               definition is installation dependent.

;;
;;                                       ?TERMINAL
;;                                       SCREEN 21 LINE 9
;;

!if 0 {
        +WORD "?terminal"
W_QTERMINAL
;    !word XQTER    ; Vector to code for ?TERMINAL
}

; ****************************************************************************
; TOGGLE

;      TOGGLE        c-addr  b  ---
;               Complement the contents of addr by the bit pattern b.

;;
;;                                       TOGGLE
;;                                       SCREEN 31 LINE 7
;;

!if 0 {
        +WORD "toggle"
W_TOGGLE
        !word *+2
;          LDA (2,X)      ; complement bits in memory address
;          EOR 0,X        ; second on stack, by pattern on
;          STA (2,X)      ; bottom of stack.
        jmp POPTWO
}

; ****************************************************************************
; TRAVERSE

;      TRAVERSE      addr1  n  ---  addr2
;               Move across the name field of a fig-FORTH variable length 
;               name field.  addr1 is the address of either the length 
;               byte or the last letter.  If n=1, the motion is toward 
;               high memory; if n= -1, the motion is toward low memory.  
;               The addr2 resulting is address of the other end of the 
;               name.

;;
;;                                       TRAVERSE
;;                                       SCREEN 39 LINE 14
;;

!if 0 {
        +WORD "traverse"
W_TRAVERSE
        !word DO_COLON
;          !word SWAP
;L1312:    !word OVER
;          !word PLUS
;          !word CLITERAL
;          !byte $7F
;          !word OVER
;          !word CAT
;          !word LESS
;          !word ZBRANCH
;L1320:    !word $FFF1    ; L1312-L1320
;          !word SWAP
;          !word DROP
        !word W_PSEMI
}

; ****************************************************************************
; UPPER

;;
;;                                       UPPER
;;                                       SCREEN 47 LINE 12
;;

!if 0 {
        +WORD "upper"
W_UPPER
        !word DO_COLON
;          !word OVER     ; This routine converts text to U case
;          !word PLUS     ; It allows interpretation from a term.
;          !word SWAP     ; without a shift-lock.
;          !word PDO
;L1950:    !word I
;          !word CAT
;          !word CLIT
;          !byte $5F
;          !word GREAT
;          !word ZBRAN
;L1956:    !word 09       ; L1961-L1956
;          !word I
;          !word CLIT
;          !byte $20
;          !word TOGGL
;L1961:    !word PLOOP
;L1962:    !word $FFEA    ; L1950-L1962
        !word W_PSEMI
}

; ****************************************************************************
; USE

;      USE           ---  addr
;               A variable containing the address of the block buffer to 
;               use next, as the least recently written.

;;
;;                                       USE
;;                                       SCREEN 58 LINE 1
;;

!if 0 {
        +WORD "use"
W_USE
        !word DO_VARIABLE
;          !word DAREA
}

; ****************************************************************************
; VOC-LINK

;      VOC-LINK      ---  addr                               U
;               A user variable containing the address of a field in the 
;               definition of the most recently created vocabulary.  All 
;               vocabulary names are linked by these fields to allow 
;               control for FORGETting through multiple vocabularies.

;;
;;                                       VOC-LINK
;;                                       SCREEN 36 LINE 9
;;

!if 0 {
        +WORD "voc-link"
W_VOCL
        !word DO_USER
;          !byte $14
}

; ****************************************************************************
; WARNING

;      WARNING       ---  addr                               U
;               A user variable containing a value controlling messages.  
;               If = 1, disc is present and screen 4 of drive 0 is the 
;               base location for messages.  If = 0, no disc is present 
;               and messages will be presented by number.  If = -1, 
;               execute (ABORT) for a user specified procedure.  See 
;               MESSAGE, ERROR.

;;
;;                                       WARNING
;;                                       SCREEN 36 LINE 6
;;

!if 0 {
        +WORD "warning"
W_WARNING
        !word DO_USER
;          !byte $E
}

; ****************************************************************************
; X

;      X
;               This is a psuedonym for the "null" or dictionary entry for 
;               a name of one character of ascii null.  It is the 
;               execution procedure to terminate interpretation of a line 
;               of text from the terminal or within a disc buffer, as both 
;               buffers always have a null at the end.

;;
;;                                       X
;;                                       SCREEN 45 LINE 11
;;                                       Actually Ascii Null
;;

!if 0 {
        +WORD "x"
W_X
        !word DO_COLON
;          !word BLK
;          !word AT
;          !word ZBRAN
;L1810:    !word $2A      ; L1830-l1810
;          !word ONE
;          !word BLK
;          !word PSTOR
;          !word ZERO
;          !word IN
;          !word STORE
;          !word BLK
;          !word AT
;          !word ZERO,BSCR
;          !word USLAS
;          !word DROP     ; fixed from model
;          !word ZEQU
;          !word ZBRAN
;L1824:    !word 8        ; L1828-L1824
;          !word QEXEC
;          !word RFROM
;          !word DROP
;L1828:    !word BRAN
;L1829:    !word 6        ; L1832-L1829
;L1830:    !word RFROM
;          !word DROP
        !word W_PSEMI
}

}
