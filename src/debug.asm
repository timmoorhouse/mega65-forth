
;;    This is a temporary trace routine, to be used until FORTH
;;    is generally operating. Then NOP the terminal query
;;    "JSR ONEKEY". This will allow user input to the text
;;    interpreter. When crashes occur, the display shows IP, W,
;;    and the word locations of the offending code. When all is
;	;    well, remove : TRACE, TCOLON, PRNAM, DECNP, and the
;;    following monitor/register equates.
;;
;;    Monitor routines needed to trace.
;;
;;XBLANK    =$D0AF         ; print one blank
;CRLF      =TCR	          ; print a carriage return and line feed.
;HEX2      =PRBYTE        ; print accum as two hex numbers
;LETTER    =ECHO         ; print accum as one ASCII character
;;ONEKEY    =$D1DC         ; wait for keystroke



W_RDUMP
        !word *+2
        stx <XSAVE
        lda #'r'
        jsr put_char_petscii
        tsx
        lda #' '
        jsr put_char_petscii
        lda $10a,x
        jsr put_hex
        lda $109,x
        jsr put_hex
        lda #' '
        jsr put_char_petscii
        lda $108,x
        jsr put_hex
        lda $107,x
        jsr put_hex
        lda #' '
        jsr put_char_petscii
        lda $106,x
        jsr put_hex
        lda $105,x
        jsr put_hex
        lda #' '
        jsr put_char_petscii
        lda $104,x
        jsr put_hex
        lda $103,x
        jsr put_hex
        lda #' '
        jsr put_char_petscii
        lda $102,x
        jsr put_hex
        lda $101,x
        jsr put_hex
        ; lda #' '
        ; jsr put_char_petscii
        ; lda $100,x
        ; jsr put_hex
        ; lda $0FF,x
        ; jsr put_hex        
        jsr CR
        ldx <XSAVE
        jmp NEXT

TRACE
!convtab scr {
        lda #' '
        jsr put_char_screencode
        lda #'w'
        jsr put_char_screencode
        lda <W+1
        jsr put_hex
        lda <W
        jsr put_hex

        lda #' '
        jsr put_char_screencode
        lda #'i'
        jsr put_char_screencode
        lda <I+1
        jsr put_hex
        lda <I
        jsr put_hex

        lda #' '
        jsr put_char_screencode
        lda #'s'
        jsr put_char_screencode
        txa
        jsr put_hex
}
        rts

;          STX XSAVE
;          JSR CRLF
;          LDA IP+1
;          JSR HEX2
;          LDA IP
;          JSR HEX2       ; print IP, the interpreter pointer
;          JSR XBLANK
;;
;;
;          LDA #0
;          LDA (IP),Y
;          STA XW
;          STA NP         ; fetch the next code field pointer
;          INY
;          LDA (IP),Y
;          STA XW+1
;          STA NP+1
;          JSR PRNAM      ; print dictionary name
;;
;          LDA XW+1
;          JSR HEX2       ; print code field address
;          LDA XW
;          JSR HEX2
;          JSR XBLANK
;;
;          LDA XSAVE      ; print stack location in zero-page
;          JSR HEX2
;          JSR XBLANK
;;
;          LDA 0,X        ;
;          JSR HEX2
;          JSR XBLANK
;;
;          LDA #1         ; print return stack bottom in page 1
;          JSR HEX2
;          TSX
;          INX
;          TXA
;          JSR HEX2
;          JSR XBLANK
;;
;;          JSR ONEKEY     ; wait for operator keystroke
;          LDX XSAVE      ; just to pinpoint early problems
;          LDY #0
;          RTS
;;
;;    TCOLON is called from DOCOLON to label each point
;;    where FORTH 'nests' one level.
;;
;TCOLON:   STX XSAVE
;          LDA W
;          STA NP         ; locate the name of the called word
;          LDA W+1
;          STA NP+1
;          JSR CRLF
;          LDA #$3A       ; ':
;          JSR LETTER
;          JSR XBLANK
;          JSR PRNAM
;          LDX XSAVE
;          RTS
;;
;;    Print name by it's code field address in NP
;;
;PRNAM:    JSR DECNP
;          JSR DECNP
;          JSR DECNP
;          LDY #0
;PN1:      JSR DECNP
;          LDA (NP),Y     ; loop till D7 in name set
;          BPL PN1
;PN2:      INY
;          LDA (NP),Y
;	   AND #$7F
;          JSR LETTER     ; print letters of name field
;          LDA (NP),Y
;          BPL PN2
;          JSR XBLANK
;          LDY #0
;          RTS

;;
;;    Decrement name field pointer
;;
;DECNP:    LDA NP
;          BNE DECNP1
;          DEC NP+1
;DECNP1:   DEC NP
;          RTS

;;
;;********************************************************
;;
;;	Apple 1 specific i/o routines
;;
;;********************************************************
;;
;; 	  input one ASCII char. to term.
;;
;INCH:	  JSR $FF00
;	  RTS
;;
;; 	  Print CR
;;
;TCR:	  LDA #$0A  ; CR
;	  JSR ECHO
;;	  LDA #$0A ; LF
;;	  JSR ECHO
;	  RTS
;;
;; 	  print one blank
;;
;XBLANK:	  LDA #$20  ; SP
;	  JSR ECHO
;	  RTS
;;
;;	  wait for keystroke
;;
;;ONEKEY:	  LDA KBDCR	; keystroke?
;;	  BPL ONEKEY
;;	  LDA KBD	; throw away key
;;	  RTS
;;
;
;; pt was here
;; original appleI code
;
;PRBYTE:	PHA
;	LSR
;	LSR
;	LSR
;	LSR
;	JSR PRHEX
;	PLA
;PRHEX:	AND #$0F
;	CMP #$0A
;	BMI PRHEX1
;	ADC #$06
;PRHEX1:
;	ADC #$30
;ECHO:	JSR $FF10
;	RTS
;