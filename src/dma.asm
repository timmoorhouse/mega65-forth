
!address {

ADDRLSB_TRIG = $d700
ADDRMSB      = $d701
ADDRBANK     = $d702  
EN018B       = $d703  
ADDRMB       = $d704
ETRIG        = $d705
ADDRLSB      = $d70e    
; TODO d711 stuff
CH0RVOL      = $d71c
CH1RVOL      = $d71d
CH2RVOL      = $d71e
CH3RVOL      = $d71f
; TODO d720 stuff
CH0BADDR     = $d721 ; 24
CH0FREQ      = $d724 ; 24
CH0TADDR     = $d727 ; 16
CH0VOLUME    = $d729
CH0CURRADDR  = $d72a ; 24
CH0TMRADDR   = $d72d ; 24
; TODO d730 stuff
CH1BADDR     = $d731 ; 24
CH1FREQ      = $d734 ; 24
CH1TADDR     = $d737 ; 16
CH1VOLUME    = $d739
CH1CURRADDR  = $d73a ; 24
CH1TMRADDR   = $d73d ; 24
; TODO d740 stuff
CH2BADDR     = $d741 ; 24
CH2FREQ      = $d744 ; 24
CH2TADDR     = $d747 ; 16
CH2VOLUME    = $d749
CH2CURRADDR  = $d74a ; 24
CH2TMRADDR   = $d74d ; 24
; TODO d750 stuff
CH3BADDR     = $d751 ; 24
CH3FREQ      = $d754 ; 24
CH3TADDR     = $d757 ; 16
CH3VOLUME    = $d759
CH3CURRADDR  = $d75a ; 24
CH3TMRADDR   = $d75d ; 24

}

;
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
;
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
;
;
;
;
;

; automatically disabled by hyppo when using a c65 rom ...
!macro dma_disable_f018b {
        lda #00
        sta EN018B
}

; automatically enabled by hyppo when using mega65 roms ...
!macro dma_enable_f018b { ; TODO use a boolean parameter?
        lda #01
        sta EN018B       
}

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

; options list

!macro dma_options .srcMB, .dstMB {
        !byte $0A ;  Request format is F018A
        !byte $80, .srcMB
        !byte $81, .dstMB
}

!macro dma_options_end { !byte $00 }

;.macro DMAStep(SourceStep, SourceStepFractional, DestStep, DestStepFractional) {
;        .if (SourceStepFractional != 0) {
;            .byte $82, SourceStepFractional
;        }
;        .if (SourceStep != 1) {
;            .byte $83, SourceStep
;        }
;        .if (DestStepFractional != 0) {
;            .byte $84, DestStepFractional
;        }
;        .if (DestStep != 1) {
;            .byte $85, DestStep
;        }		
;}

;.macro DMADisableTransparency() {
;        .byte $06
;}
;
;.macro DMAEnableTransparency(TransparentByte) {
;        .byte $07 
;        .byte $86, TransparentByte
;}

; job list

!macro dma_job_fill .value, .dst, .len, .chain {
        !if .chain {
                !byte $07 ; Fill and chain
        } else {
                !byte $03 ; Fill and last request
        }	

        !word .len
        !byte .value
        !byte $00
        !byte $00
        !word .dst & $ffff
        !byte ((.dst >> 16) & $0f) 
        !word $0000 ; modulo
}

!macro dma_job_copy .src, .dst, .len, .chain, .backwards {
        !if .chain {
                !byte $04 ; Copy and chain
        } else {
                !byte $00 ; Copy and last request
        }	
     
        !set backByte = 0
        !if .backwards {
                !set backByte = $40
                !set .src = .src + .len - 1
                !set .dst = .dst + .len - 1
        }
        !word .len

        !word .src & $ffff
        !byte (.src >> 16) + backByte

        !word .dst & $ffff
        !byte ((.dst >> 16) & $0f)  + backByte
        !word $0000 ; modulo
}

;.macro DMAMixJob(Source, Destination, Length, Chain, Backwards) {
;    .byte $00 // No more options
;    .if (Chain) {
;        .byte $04 // Mix and chain
;    } else {
;        .byte $00 // Mix and last request
;    }	
;    
;    .var backByte = 0
;    .if (Backwards) {
;        .eval backByte = $40
;        .eval Source = Source + Length - 1
;        .eval Destination = Destination + Length - 1
;    }
;    .word Length // Size of Copy
;    .word Source & $ffff
;    .byte [Source >> 16] + backByte
;    .word Destination & $ffff
;    .byte [[Destination >> 16] & $0f]  + backByte
;    .if (Chain) {
;        .word $0000
;    }
;}

