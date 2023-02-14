

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

;.macro VIC4_SetCharLocation(addr) {
;    lda #[addr & $ff]
;    sta $d068
;    lda #[[addr & $ff00]>>8]
;    sta $d069
;    lda #[[addr & $ff0000]>>16]
;    sta $d06a
;}

;.macro VIC4_SetScreenLocation(addr) {
;    lda #[addr & $ff]
;    sta $d060
;    lda #[[addr & $ff00]>>8]
;    sta $d061
;    lda #[[addr & $ff0000]>>16]
;    sta $d062
;    lda #[[[addr & $ff0000]>>24] & $0f]
;    sta $d063
;}
