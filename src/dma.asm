
!address {

ADDRLSB_TRIG = $d700
ADDRMSB      = $d701
ADDRBANK     = $d702  
EN018B       = $d703  
ADDRMB       = $d704
ETRIG        = $d705
ADDRLSB      = $d70e    

}

; DMA list:
;  $00 command LSB
;  $01 count LSB
;  $02 count MSB
;  $03 source address LSB
;  $04 source address MSB
;  $05 source address bank and flags
;  $06 destination address LSB
;  $07 destination address MSB
;  $08 destination address bank and flags
;  $09 modulo LSB (F018)          command MSB (F018B)
;  $0A modulo MSB (F018)          modulo LSB / mode (F018B)
;  $0B                            modulo MSB / mode (F018B)
;
; Command word
;  0-1  DMA operation type
;  2    chain (another DMA list follows)
;  3    yield to interrupts
;  4    MINTERM -SA,-DA bit
;  5    MINTERM -SA,DA bit
;  6    MINTERM SA,-DA bit
;  7    MINTERM SA,DA bit
;  8-9  addressing mode of source
;  10-11 addressing mode of destination
;  12-15 reserved (set to zero)
;
; Command field
;  %00  copy
;  %01  mix (via MINTERMs)
;  %02  swap
;  %03  fill
dma_cmd_copy = 0
dma_cmd_mix  = 1
dma_cmd_swap = 2
dma_cmd_fill = 3

dma_cmd_chain = 4

; Addressing modes
;  %00  linear (normal)
;  %01  modulo (rectangular)
;  %02  hold (constant address)
;  %03  XY mod (bitmap rectangular) (not yet implemented?)
;
; Bank and flags
;  0-3  memory bank
;  4    hold (do not change address)
;  5    modulo (apply modulo field)
;  6    direction (if set, decrement address instead of incrementing)
;  7    I/O (if set, I/O address are visible at $d000-$dfff during DMA)

!macro dma_run .list {
        lda #0
        sta ADDRMB
        lda #((.list >> 16) & $0f)
        sta ADDRBANK
        lda #>.list
        sta ADDRMSB
        lda #<.list
        sta ETRIG ; LSB
}

!macro dma_inline {
        sta $d707
}

; options list

!macro dma_options .srcMB, .dstMB {
        !byte $0A ;  Request format is F018A
  !if .srcMB {
        !byte $80, .srcMB
  }
  !if .dstMB {
        !byte $81, .dstMB
  }
}

!macro dma_options_end {
        !byte $00 
}
