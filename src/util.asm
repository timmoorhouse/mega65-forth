
!address {
; TODO where to put this stuff?
MULTINA = $d770 ; 32
MULTINB = $d774 ; 32
MULTOUT = $d778 ; 64
DIVOUT  = $d768 ; 64
}


!macro upstart .addr {
 
        !word +
        !word 10
        !byte $fe, $02, '0'             ; BANK 0
        !byte ':'
        !byte $9e                       ; SYS
        !byte '0' + .addr % 10000 / 1000
        !byte '0' + .addr %  1000 /  100
        !byte '0' + .addr %   100 /   10
        !byte '0' + .addr %    10
        !byte $00

+       !word 0                         ; End of basic terminators

}

!macro map_reset {
        lda #$00
        tax 
        tay 
        taz 
        map
        ; eom        
}

!macro enable40MHz {
        lda #$41
        sta $00                         ; 40 MHz mode
}


;.macro disableC65ROM() {
;        lda #$70
;        sta $d640 ; HTRAP00
;        eom
;}

;.macro mapMemory(source, target) {
;    .var sourceMB = (source & $ff00000) >> 20
;    .var sourceOffset = ((source & $00fff00) - target)
;    .var sourceOffHi = sourceOffset >> 16
;    .var sourceOffLo = (sourceOffset & $0ff00 ) >> 8
;    .var bitLo = pow(2, (((target) & $ff00) >> 12) / 2) << 4
;    .var bitHi = pow(2, (((target-$8000) & $ff00) >> 12) / 2) << 4
;    
;    .if (target<$8000) {
;        lda #sourceMB
;        ldx #$0f
;        ldy #$00
;        ldz #$00
;    } else {
;        lda #$00
;        ldx #$00
;        ldy #sourceMB
;        ldz #$0f
;    }
;    map 
;
;    //Set offset map
;    .if (target<$8000) {
;        lda #sourceOffLo
;        ldx #[sourceOffHi + bitLo]
;        ldy #$00
;        ldz #$00
;    } else {
;        lda #$00
;        ldx #$00
;        ldy #sourceOffLo
;        ldz #[sourceOffHi + bitHi]
;    }	
;    map 
;    eom
;}
