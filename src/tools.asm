
; ****************************************************************************
; TOOLS

; ****************************************************************************
; .S
; (--)
; ANSI 15.6.0220

; TODO flip the order around!!!!!!

; see TRAVERSE (FIG)
        +WORD ".s"
W_DOTS
        !word *+2

        ; TODO rewrite in FORTH using DEPTH, ., DO, LOOP
        ; jsr CR
!convtab scr {
        lda #'<'
        jsr put_char_screencode
}
        stx <XSAVE
        lda #TOS
        sec
        sbc XSAVE
        lsr
        jsr put_hex
!convtab scr {
        lda #'>'
}
        jsr put_char_screencode

-       cpx #TOS
        beq +

!convtab scr {
        lda #' '
}
        jsr put_char_screencode
        lda 1,x
        jsr put_hex
        lda 0,x
        jsr put_hex
        inx
        inx
        jmp -

+       ldx <XSAVE

!if 0 {
!convtab scr {
        lda #' '
        jsr put_char_screencode
        lda #'o'
        jsr put_char_screencode
        lda #'k'
        jsr put_char_screencode
}
}
        jsr CR

        jmp NEXT

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
!if 0 {
        +WORD "?"
W_QUES
        !word DO_COLON
;          !word AT
;          !word DOT
        !word W_SEMI
}

; ****************************************************************************
; DUMP
; (addr u --)
; ANSI 15.6.1.1280
!if 0 {
        +WORD "dump"
W_DUMP
        !word *+2
        rts
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
!if 0 {
        +WORD "see"
W_SEE
        !word *+2
        rts
}

; ****************************************************************************
; WORDS
; (--)
; ANSI 15.6.1.2465
        ; TODO rewrite in FORTH using NAME>STRING, TYPE, TRAVERSE-WORDLIST

        +WORD "words"
W_WORDS
        !word *+2

WORDS
        lda HERE
        sta <WORDP
        lda HERE+1
        sta <WORDP+1


-       lda <WORDP
        ora <WORDP+1
        bne +

        ; +TRACE
        ; brk ; TODO
        jmp NEXT

        ; we've got a word

+       clc
        lda #2
        adc <WORDP
        sta <STRING
        lda #0
        adc <WORDP+1
        sta <STRING+1

        ; lda #' '
        ; jsr put_char_screencode
        ; lda <WORDP+1
        ; jsr put_hex
        ; lda <WORDP
        ; jsr put_hex

        ; lda #' '
        ; jsr put_char_screencode
        ; lda <STRING+1
        ; jsr put_hex
        ; lda <STRING
        ; jsr put_hex

!convtab scr {
        lda #' '
        jsr put_char_screencode
}
        jsr put_string

        ldy #0
        lda (<WORDP),y
        taz
        iny
        lda (<WORDP),y
        stz <WORDP
        sta <WORDP+1

        jmp -

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
        !word W_SEMI
}
