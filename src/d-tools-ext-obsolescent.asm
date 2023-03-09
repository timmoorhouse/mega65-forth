
; ****************************************************************************
; FORGET
; ("text" --)
; ANSI 15.6.2.1580   (obsolescent)

; FIG:
;      FORGET                                                E,L0
;               Executed in the form:
;                         FORGET  cccc
;               Deletes definition named cccc from the dictionary with all 
;               entries physically following it.  In fig-FORTH, an error 
;               message will occur if the CURRENT and CONTEXT vocabularies 
;               are not currently the same.
;
;;
;;                                       FORGET
;;                                       Altered from model
;;                                       SCREEN 72 LINE 6

!if ENABLE_TOOLS_EXT {
        +CREATE "forget", 0
W_FORGET
        !word DO_COLON
;          !word TICK,NFA,DUP
;          !word FENCE,AT,ULESS,CLIT
;          !byte $15
;          !word QERR,TOR,VOCL,AT
;L3220:    !word R,OVER,ULESS
;          !word ZBRAN,L3225-*
;          !word FORTH,DEFIN,AT,DUP
;          !word VOCL,STORE
;          !word BRAN,$FFFF-24+1 ; L3220-*
;L3225:    !word DUP,CLIT
;          !byte 4
;          !word SUB
;L3228:    !word PFA,LFA,AT
;          !word DUP,R,ULESS
;          !word ZBRAN,$FFFF-14+1 ; L3228-*
;          !word OVER,TWO,SUB,STORE
;          !word AT,DDUP,ZEQU
;          !word ZBRAN,$FFFF-39+1 ; L3225-*
;          !word RFROM,DP,STORE
        !word W_PSEMI
}
