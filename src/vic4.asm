
!address {
vic4_KEY             = $d02f
LINESTEPLSB     = $d058 
LINESTEPMSB     = $d059
CHRCOUNT        = $d05e ; LSB, MSB in SCRNPTR+3 (D063)
SCRNPTRLSB      = $d060 
SCRNPTRMSB      = $d061
SCRNPTRBNK      = $d062
SCRNPTRMB       = $d063 ; 28, last contains EXGLYPH, CHARCOUNT too
COLPTRLSB       = $d064
COLPTRMSB       = $d065
DISPROWS        = $d07b ; TODO name? book has just "number"
}

!macro vic4_enable {
        lda #$47	            
        sta vic4_KEY
        lda #$53
        sta vic4_KEY
}
