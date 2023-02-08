
; TODO
; as a starting point:
; - take a pass over the BASIC keywords and make equivalents? (will want DIR, etc)
; - look at C64 forth implementations (eg superFORTH)


; ****************************************************************************
; MON

;      MON
;               Exit to the system monitor, leaving a re-entry to Forth, 
;               if possible.

;;
;;                                       MON
;;                                       SCREEN 79 LINE 3
;;
!if 0 {
;NTOP ???????
        +WORD "mon"
W_MON
        !word *+2
;          STX XSAVE
;          BRK       ; break to monitor which is assumed
;          LDX XSAVE ; to save this as reentry point
        jmp NEXT
}
