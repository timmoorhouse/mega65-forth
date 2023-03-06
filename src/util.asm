
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
