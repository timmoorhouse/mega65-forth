
; ****************************************************************************
; TOOLS

; ****************************************************************************
; .S
; (--)
; ANSI 15.6.0220

; TODO flip the order around!!!!!!
; TODO rewrite in FORTH using DEPTH, ., DO, LOOP

!if ENABLE_TOOLS {
        +WORD ".s"
W_DOTS
        !word *+2

        stx <XSAVE
        lda #' '
        jsr put_char
        lda #'<'
        jsr put_char
        lda #TOS
        sec
        sbc XSAVE
        lsr
        jsr put_hex
        lda #'>'
        jsr put_char

        lda #TOS
        sta <TEMP1

-       lda <TEMP1
        cmp <XSAVE
        beq +

        lda #' '
        jsr put_char
        dec <TEMP1
        dec <TEMP1
        ldy <TEMP1
        lda base_page+1,y
        jsr put_hex
        ldy <TEMP1
        lda base_page,y
        jsr put_hex
        jmp -
+        

        jmp NEXT
}

; ****************************************************************************
; ?
; (a-addr --)
; ANSI 15.6.1.0600

; FIG:
;      ?             addr  ---                               L0
;               Print the value contained at the address in free format 
;               according to the current base.
;
;;
;;                                       ?
;;                                       SCREEN 76 LINE 11

; TODO always enable?

!if ENABLE_TOOLS {
!if 0 {
        +WORD "?"
W_QUES
        !word DO_COLON
;          !word AT
;          !word DOT
        !word W_PSEMI
}
}

; ****************************************************************************
; DUMP
; (addr u --)
; ANSI 15.6.1.1280

!if ENABLE_TOOLS {
!if 0 {
        +WORD "dump"
W_DUMP
        !word *+2
        rts
}
}

; ****************************************************************************
; SEE
; ("text" --)
; ANSI 15.6.1.2194

; FIG
;      DUMP          addr  n  ---                            L0
;               Print the contents of n memory locations beginning at 
;               addr.  Both addresses and contents are shown in the 
;               current numeric base.

!if ENABLE_TOOLS {
!if 0 {
        +WORD "see"
W_SEE
        !word *+2
        rts
}
}

; ****************************************************************************
; WORDS
; (--)
; ANSI 15.6.1.2465

!if ENABLE_TOOLS {
        +WORD "words"
W_WORDS
        !word DO_COLON
        !word W_CR
        +LITERAL W_PRINT_NAME
        !word W_FORTH_WORDLIST
        !word W_TRAVERSE_WORDLIST
        !word W_PSEMI

W_PRINT_NAME
        !word DO_COLON
        !word W_OUT
        !word W_CAT
        !word W_DUP
        +ZBRANCH +
        !word W_SPACE
+       !word W_CSLL
        !word W_GREATER
        +ZBRANCH +
        !word W_CR
+       !word W_NAME_TO_STRING
        !word W_TYPE
        !word W_TRUE
        !word W_PSEMI

; FIG
;      VLIST
;               List the names of the definitions in the context 
;               vocabulary.  "Break" will terminate the listing.
;
;;
;;                                       VLIST
;;                                       SCREEN 78 LINE 2
;;
;;
!if 0 {
;        +WORD "vlist"
W_VLIST
        !word DO_COLON

;          !word CLIT
;          !byte $80
;          !word OUT
;          !word STORE

;          !word CON
;          !word AT
;          !word AT

;L3706:    !word OUT
;          !word AT
;          !word CSLL
;          !word GREAT
;          !word ZBRANCH
;L3711:    !word $A       ; L3716-L3711
;          !word CR
;          !word ZERO
;          !word OUT
;          !word STORE

;L3716:    !word DUP
;          !word IDDOT
;          !word SPACE
;          !word SPACE
;          !word PFA
;          !word LFA
;          !word AT
;          !word DUP
;          !word ZEQU
;          !word QTERM
;          !word OR
;          !word ZBRANCH
;L3728:    !word $FFD4    ; L3706-L3728

;          !word DROP
        !word W_PSEMI
}
}
