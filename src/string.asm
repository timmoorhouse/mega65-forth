
; ****************************************************************************
; STRING

; ****************************************************************************
; -TRAILING
; (c-addr u_1 -- c-addr u_2)
; ANSI 17.6.1.0170

; FIG:
;
;
;      -TRAILING     addr  n1  ---  addr  n2
;               Adjusts the character count n1 of a text string beginning 
;               address to suppress the output of trailing blanks.  ie. 
;               the characters at addr+n2 are blanks.
;
;;
;;                                       -TRAILING
;;                                       SCREEN 44 LINE 5

!if ENABLE_STRING {
        +WORD "-trailing"
W_DTRAILING
        !word DO_COLON
;          !word DUP
;          !word ZERO
;          !word PDO
;L1663:    !word OVER
;          !word OVER
;          !word PLUS
;          !word ONE
;          !word SUB
;          !word CAT
;          !word BL
;          !word SUB
;          !word ZBRAN
;L1672:    !word 8        ; L1676-L1672
;          !word LEAVE
;          !word BRAN
;L1675:    !word 6        ; L1678-L1675
;L1676:    !word ONE
;          !word SUB
;L1678:    !word PLOOP
;L1679:    !word $FFE0    ; L1663-L1679
        !word W_SEMI
}

; ****************************************************************************
; /STRING
; (c-addr_1 u_1 n -- c-addr_2 u_2)
; ANSI 17.6.1.0245

!if ENABLE_STRING {
}

; ****************************************************************************
; BLANK
; (c-addr u --)
; ANSI 17.6.1.0780

; FIG
;      BLANKS        addr  count  ---
;               Fill an area of memory begining at addr with blanks.
;

;;
;;                                       BLANKS
;;                                       SCREEN 46 LINE 7
;;

!if ENABLE_STRING {
!if 0 {
        +WORD "blanks"
W_BLANK
        !word DO_COLON
        !word W_BL
        !word W_FILL
        !word W_SEMI
}
}

; ****************************************************************************
; CMOVE
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0910

; FIG:
;
;      CMOVE         from  to  count  ---
;               Move the specified quantity of bytes beginning at address 
;               from to address to.  The contents of address from is moved 
;               first proceeding toward high memory.  Further 
;               specification is necessary on word addressing computers.
;
;;
;;                                       CMOVE
;;                                       SCREEN 22 LINE 1

!if ENABLE_STRING {
        +WORD "cmove"
W_CMOVE
        !word *+2
;          LDA #3
;          JSR SETUP
;L370:     CPY N
;          BNE L375
;          DEC N+1
;          BPL L375
;          JMP NEXT
;L375:     LDA (N+4),Y
;          STA (N+2),Y
;          INY
;          BNE L370
;          INC N+5
;          INC N+3
;          JMP L370
}

; ****************************************************************************
; CMOVE>
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0920

!if ENABLE_STRING {
}

; ****************************************************************************
; COMPARE
; (c-addr_1 u_1 c-addr_2 u_2 -- n)
; ANSI 17.6.1.0935

; The word itself is required by the implmentation (of FIND) but is only visible if SEARCH is enabled

!if ENABLE_STRING {
        +WORD "compare"
}
W_COMPARE
        !word DO_COLON
        ; TODO
        !word W_SEMI

; ****************************************************************************
; SEARCH
; (c-addr_1 u_1 c-addr_2 u_2 -- c-addr_3 u_3 flag)
; ANSI 17.6.1.2191

!if ENABLE_STRING {
}

; ****************************************************************************
; SLITERAL
; (???)
; ANSI 17.6.1.2212

!if ENABLE_STRING {
}
