
!cpu m65
!convtab pet

; TODO clean up the symbol names - what does acme allow?

!ifndef ENABLE_BLOCK                    { ENABLE_BLOCK                 = 0 }
!ifndef ENABLE_BLOCK_EXT                { ENABLE_BLOCK_EXT             = 0 }
!if 1                                   { ENABLE_CORE                  = 1 } ; Required
!ifndef ENABLE_CORE_EXT                 { ENABLE_CORE_EXT              = 1 }
!ifndef ENABLE_CORE_EXT_OBSOLESCENT     { ENABLE_CORE_EXT_OBSOLESCENT  = 0 }
!ifndef ENABLE_DOUBLE                   { ENABLE_DOUBLE                = 1 }
!ifndef ENABLE_DOUBLE_EXT               { ENABLE_DOUBLE_EXT            = 1 }
!ifndef ENABLE_EXCEPTION                { ENABLE_EXCEPTION             = 0 }
!ifndef ENABLE_EXCEPTION_EXT            { ENABLE_EXCEPTION_EXT         = 0 }
!ifndef ENABLE_FACILITY                 { ENABLE_FACILITY              = 1 }
!ifndef ENABLE_FACILITY_EXT             { ENABLE_FACILITY_EXT          = 1 }
!ifndef ENABLE_FIG                      { ENABLE_FIG                   = 1 } ; TODO remove
!ifndef ENABLE_FILE                     { ENABLE_FILE                  = 1 }
!ifndef ENABLE_FILE_EXT                 { ENABLE_FILE_EXT              = 1 }
!ifndef ENABLE_FLOATING                 { ENABLE_FLOATING              = 0 }
!ifndef ENABLE_FLOATING_EXT             { ENABLE_FLOATING_EXT          = 0 }
!ifndef ENABLE_GFORTH                   { ENABLE_GFORTH                = 1 } ; useful things following gforth's extensions
!ifndef ENABLE_LOCAL                    { ENABLE_LOCAL                 = 0 }
!ifndef ENABLE_LOCAL_EXT                { ENABLE_LOCAL_EXT             = 0 }
!ifndef ENABLE_LOCAL_EXT_OBSOLESCENT    { ENABLE_LOCAL_EXT_OBSOLESCENT = 0 }
!ifndef ENABLE_MEGA65                   { ENABLE_MEGA65                = 1 }
!ifndef ENABLE_MEMORY                   { ENABLE_MEMORY                = 0 }
!ifndef ENABLE_MEMORY_EXT               { ENABLE_MEMORY_EXT            = 0 }
!ifndef ENABLE_SEARCH                   { ENABLE_SEARCH                = 1 }
!ifndef ENABLE_SEARCH_EXT               { ENABLE_SEARCH_EXT            = 1 }
!ifndef ENABLE_STRING                   { ENABLE_STRING                = 1 }
!ifndef ENABLE_STRING_EXT               { ENABLE_STRING_EXT            = 1 }
!ifndef ENABLE_TOOLS                    { ENABLE_TOOLS                 = 1 } ; for words, .s, etc
!ifndef ENABLE_TOOLS_EXT                { ENABLE_TOOLS_EXT             = 1 }
!ifndef ENABLE_TOOLS_EXT_OBSOLESCENT    { ENABLE_TOOLS_EXT_OBSOLESCENT = 0 }
!ifndef ENABLE_XCHAR                    { ENABLE_XCHAR                 = 0 }
!ifndef ENABLE_XCHAR_EXT                { ENABLE_XCHAR_EXT             = 0 }

!ifndef DEBUG                           { DEBUG                        = 0 }
!ifndef ENABLE_RUNTIME_CHECKS           { ENABLE_RUNTIME_CHECKS        = 0 } ; TODO lots of things are triggering this - need to clean them up before enabling
!ifndef USE_BASIC                       { USE_BASIC                    = 0 } ; not used - REMOVE?


;
; Colour scheme
; These are indexes into the colour pallen (see docs for BACKGROUND for a table)
;

COLOUR_OUTPUT =   1 ; white
COLOUR_INPUT  =   7 ; yellow
COLOUR_PROMPT =  14 ; lt blue
COLOUR_ERROR  =   4 ; purple

;
; TODO does it make sense to use the basic rom at all? I'm wondering about math routines in particular, but there
; may not be a good way to use them (no jump vectors to them so they could move) - might be able to execute a token for them

; TODO inw/dew to increment/decrement words!
; TODO inq/deq for quads
; TODO asw for asl on a word!
; TODO row for rol on a word!
; TODO neg for xor $ff
; TODO phw to push a word? only out of memory (maybe not so useful) or immediate (could be useful)
; TODO quad stuff for double precision things

; Alignment:
; - VARIABLEs must be aligned
; - CREATE must give an aligned data field?
; - After definitions are compled the data-space pointer must be aligned

!source "util.asm"
!source "dma.asm"
!source "vic4.asm"
!source "gpio.asm"

* = $2001
        +upstart entry
entry
!if 0 {
        ; works ... into the monitor
        ; TODO is it getting the bank from $0002?
        ;lda #0
        ;sta $011d
        ;sta $011f
        brk
        ; TODO resume using 'G' in monitor
}
!if 0 {
        ; see vector at $0315, $032e
        jmp $cd7c
}

        jmp COLD

!source "basepage.asm"


; VM Registers
; S - data stack pointer
; R - return stack pointer
; I - instruction pointer (FIG uses IP)
; W - word pointer (to definition currently executing, used to get parameter field)
; U - user pointer (in multitasked implementations, pointer to currently executing task) (FIG uses UP)
;
; floating point stack is allowed to be on the data stack or separate
; (might want to look at using the math register area directly?)


!macro STRING .text {
        !byte len(.text)
        !text .text
}


; Control bits:
; - fig always sets bit 8 (so we can find the start of the name crawling back from the code field)
; - precedence (fig uses bit 7 for immediate)
; - smudge (fig uses bit 6 for hidden)
; - length uses bits 1-5

F_END_MARKER = $80
F_IMMEDIATE  = $40
F_HIDDEN     = $20

!macro ALIGN {
  !if *&1 {
        !byte 0
  }
}

!macro NONAME {
        +ALIGN
}

!set _here = $0

!macro WORD2 .name, .flags {
        ; TODO align so that code field never straddles a page
        +ALIGN
        !word _here
        !set _here = *-2
        !byte len(.name) | F_END_MARKER | .flags ; TODO control bits
        !text .name
}

!macro WORD .name {
        +WORD2 .name, 0
}

!macro WORD_IMM .name {
        +WORD2 .name, F_IMMEDIATE
}

; +WORD "name"
; !word <code field> - DO_COLON, DO_CONSTANT, DO_USER
; [!word <parameter>...]

!ifdef DEBUG                { !src "debug.asm"         }

; Dictionary entries
;   FIG:
;     - count of name with high order bits for control bits
;     - name string with last byte having MSB set
;     - link
;     - code address
;       - DO_USER for words in user area
;         - byte $nn
;       - DO_CONSTANT for constants
;         - !word $nnnn
;       - DO_COLON ???
;         - ...
;         - SEMIS | PSCOD | QUIT
;         - weird cases: INTERPRET (fig)
;       - DO_DOES - FORTH in search-ext
;       - XCR (for CR in core)
;       - XQTER (for ?TERMINAL in fig)
;       - .word $*+2 for ML
;         - word *+2
;         - ...
;         - JMP POPTWO/POP/PUSH/PUT/NEXT | BUMP (see 0BRANCH) | PL2 (see (LOOP))
;         - weird cases: AND, EXECUTE, D-, BRANCH, COLD
;     - parameter field


;SSIZE     = 128                         ; sector size in bytes
;NBUF      = 8                           ; number of buffers desired in RAM
;                                        ; (SSIZE*NBUF >= 1024 bytes)
;SECTR     = 800                         ; sector per drive
;                                        ; forcing high drive to zero
;SECTL     = 1600                        ; sector limit for two drives
;                                        ; of 800 per drive.
;BMAG      = 1056                        ; total buffer magnitude, in bytes
;                                        ; expressed by (SSIZE+4)*NBUF
;DAREA     = UAREA-BMAG                  ; disk buffer space.

TIBX      = $0100                       ; terminal input buffer of 84 bytes.
;ORIG      = $0300                       ; origin of FORTH's Dictionary.

; TODO sort out memory layout, what ROM bits we need, etc
UAREA_LEN  = 128 ; TODO shrink this?
UAREA      = $8000 - UAREA_LEN

;    From DAREA downward to the top of the dictionary is free
;    space where the user's applications are compiled.
;
;    Boot up parameters. This area provides jump vectors
;    to Boot up  code, and parameters describing the system.

;POP4
;        inx
;        inx
;        ; jmp POP3

POP3
        inx
        inx
        ; jmp POP2

POP2
        inx
        inx
        ; jmp POP

POP
        inx
        inx
        jmp NEXT

        ; A + top of stack is the 16-bit value
PUSH
        dex
        dex
        ; jmp PUT

PUT

        sta 1,x
        pla
        sta 0,x
        ; jmp NEXT

NEXT
        ; Execute the word with the code field pointed to by I
        ; (in the current stack frame)
        ldy #1
        lda (<I),y     ; Fetch code field address pointed
        sta <W+1        ; to by IP.
        dey
        lda (<I),y
        sta <W

        inw I           ; Increment I by two
        inw I

        ; After the jmp:
        ; - X contains the data stack pointer (this should always be preserved)
        ; - Y contains 0 (this can be trashed) TODO should we depend on this?
        ; - A & Z contain nothing in particular (and can be trashed) TODO can we use z for anything?
        jmp &DO_JUMP_W

DO_COLON
        ; ldy #0 ; TODO

        ; Start executing the word with the code field pointed to by W
        ; (in a new stack frame)
!if 1 {
        lda <I+1 ; push I
        pha
        lda <I
        pha
} else {
        phw &I ; TODO why doesn't this work?
}

        clc ; ???
        lda <W ; I = W + 2
        adc #2
        sta <I
        tya
        adc <W+1
        sta <I+1
        jmp NEXT

DO_CONSTANT
        ldy #2
        lda (<W),y
        pha
        iny
        lda (<W),y
        jmp PUSH

; 00: latest???
; 02: backspace???
; 04 ??? UAP start of user area?
U_S0    = $06   ; S0 (see internal SP! in core)
U_R0    = $08   ; R0 (see internal RP! in core)
U_TIB   = $0a   ; TIB (core-ext)
;           - 0C: WIDTH (fig)
;           - 0E: WARNING (fig)
; U_FENCE = $10   ; FENCE (fig)
; U_DP    = $12   ; DP (fig)
;           - 14: VOC-LINK (fig)
U_BLK   = $16   ; BLK (block)
; U_IN    = $18   ; >IN (fig)
;           - 1A: OUT (fig)
;           - 1C: SCR (block-ext)
;           - 1E: OFFSET (fig)
;           - 20: CONTEXT (fig)
;           - 22: CURRENT (fig)
U_STATE = $24   ; STATE (core) TODO move to base page
; U_BASE  = $26   ; BASE (core) TODO move to base page
; U_DPL   = $28   ; DPL (fig)
;           - 2A: FLD (fig)
;           - 2C: CSP (fig)
;           - 2E: R# (fig)
;           - 30: HLD (fig)
; Things not in FIG ...

DO_USER ; TODO REMOVE
        ldy #2
        clc
        lda (<W),y
        adc <U
        pha
        lda #0
        adc <U+1
        jmp PUSH

DO_VARIABLE
        ; ldy #0 ; TODO
        clc
        lda <W
        adc #2
        pha
        tya
        adc <W+1
        jmp PUSH

DO_DOES
;       LDA IP+1
;       PHA
;       LDA IP
;       PHA
;       LDY #2
;       LDA (W),Y
;       STA IP
;       INY
;       LDA (W),Y
;       STA IP+1
;       CLC
;       LDA W
;       ADC #4
;       PHA
;       LDA W+1
;       ADC #0
        jmp PUSH

!src "kernel.asm"
!src "internals.asm"
!source "console.asm"

; ****************************************************************************
; COLD

;        +WORD "cold"
;W_COLD
;        !word *+2
COLD
;      COLD
;               The cold start procedure to adjust the dictionary pointer
;               to the minimum standard and restart via ABORT.  May be
;               called from the terminal to remove application programs
;               and restart.

!if 1 {
        +map_reset

        ; E000-FFFF     3E000   KERNAL
        ; C000-DFFF
        ; A000-BFFF
        ; 8000-9FFF     20000   DOS ?
        ; 6000-7FFF
        ; 4000-5FFF
        ; 0000-1FFF

        ldy #$00        ; Map in KERNAL
        ldz #$83
        lda #$00
        ldx #$00
        map

!if USE_BASIC {
        ; TODO
}
        +vic4_enable
        +enable40MHz
        ; TODO bank I/O in
        lda #$35
        sta $01
        ; +dma_enable_f018b ; TODO not needed?

        eom
}

        ; set our base page
        lda #>base_page
        tab
        
; rest of cold stuff from FIG ...
;          LDA ORIG+$0C   ; from cold start area
;          STA FORTH+6 ; top of stack?
;          LDA ORIG+$0D
;          STA FORTH+7


;          LDY #$15
;          BNE +
WARM
;          LDY #$0F
;+

!if 1 { ; TODO REMOVE
        ; Set up user area
        lda #<UAREA
        sta <U
        lda #>UAREA
        sta <U+1
}

        ; Save return stack pointer in R0
        stx <XSAVE
        tsx
        stx UAREA+U_R0
        ldx <XSAVE
        tsy
        sty UAREA+U_R0+1

        ; Reposition data stack
        ldx #TOS

        ; Save data stack pointer in S0
        stx UAREA+U_S0
        ldy #0
        sty UAREA+U_S0+1

        lda #<TIBX
        sta UAREA+U_TIB
        lda #>TIBX
        sta UAREA+U_TIB+1

        lda INITIAL_FORTH_WORDLIST
        sta FORTH_WORDLIST
        lda INITIAL_FORTH_WORDLIST+1
        sta FORTH_WORDLIST+1

        lda #<INITIAL_HERE
        sta <HERE
        lda #>INITIAL_HERE
        sta <HERE+1

        ; TODO set WIDTH
        ; TODO set WARNING
        ; TODO set FENCE
        ; TODO set DP
        ; TODO set VOC-LINK ; will be set by ABORT
        ; BLK set in QUIT
        ; TODO set IN
        ; TODO set OUT
!if ENABLE_BLOCK {
        ; TODO set SCR
}
        ; TODO set OFFSET
        ; TODO set CONTEXT
        ; TODO set CURRENT
        ; STATE set in QUIT
        ; BASE set in ABORT
        ; TODO set DPL
        ; TODO set FLD
        ; TODO set CSP
        ; TODO set R#
        ; TODO set HLD

        ; ldy #0 ; still 0 from above
        sty <SOURCE_ID
        sty <SOURCE_ID+1

        cld

!if DEBUG {
W_STARTUP = W_STARTUP_DEBUG
} else {
W_STARTUP = W_ABORT
}

        ; TODO just do W_ABORT directly ...
        lda #<W_STARTUP+2
        sta <I
        lda #>W_STARTUP+2
        sta <I+1

        lda #14 ; Lower case
        jsr EMIT
        lda #11 ; Disable shift-mega case changes
        jsr EMIT

        ; TODO a FONT word that switches font
        +dma_run _install_font_dmalist

        jsr PAGE

        lda #3 ; cyan
        jsr FOREGROUND
        lda #<_startup_text1
        sta <STRING
        lda #>_startup_text1
        sta <STRING+1
        jsr put_string
!ifdef HAVE_REVISION {
        lda #'-'
        jsr EMIT
        lda #<_revision
        sta <STRING
        lda #>_revision
        sta <STRING+1
        jsr put_string
}
        jsr CR

        lda #14 ; lt blue
        jsr FOREGROUND

        lda #<_startup_text2
        sta <STRING
        lda #>_startup_text2
        sta <STRING+1
        jsr put_string
        jsr CR

        ; just in case the colour scheme is disabled ...
        lda #1 ; white
        jsr FOREGROUND

        ; ldy #0

        jmp NEXT

!if DEBUG {
        +NONAME
W_STARTUP_DEBUG
        !word DO_COLON
;TEST
!if 0 {
        !word W_WORDS
        !word W_CR
}
!if 0 {
        !word W_FORTH_WORDLIST,W_AT,W_DOT,W_CR
        !word W_HERE,W_DOT,W_CR
}
!if 0 {
        !word W_COMPARE_TEST
}
!if ENABLE_FILE {
        !word W_FILE_TEST
}
!if 0 {
        !word W_TONUMBER_TEST
}
        !word W_ABORT
}

_startup_text1
        +STRING "MEGA65-Forth 0.1"
_startup_text2
        +STRING "bye will exit to BASIC\r"
!ifdef HAVE_REVISION {
!src "revision.asm"
}
_install_font_dmalist
        +dma_options $00, $ff
        +dma_options_end
        +dma_job_copy $0029000, $ff7e000, $1000, 0, 0
; A: 029000
; B: 03D000
; C: 02D000


!src "block.asm"
!src "block-ext.asm"
!src "core.asm"
!src "core-ext.asm"
!src "core-ext-obsolescent.asm"
!src "double.asm"
!src "double-ext.asm"
!src "exception.asm"
!src "exception-ext.asm"
!src "facility.asm"
!src "facility-ext.asm"
!src "fig.asm"
!src "file.asm"
!src "file-ext.asm"
!src "floating.asm"
!src "floating-ext.asm"
!src "gforth.asm"
!src "local.asm"
!src "local-ext.asm"
!src "local-ext-obsolescent.asm"
!src "mega65.asm"
!src "memory.asm"
!src "memory-ext.asm"
!src "search.asm"
!src "search-ext.asm"
!src "string.asm"
!src "string-ext.asm"
!src "tools.asm"
!src "tools-ext.asm"
!src "tools-ext-obsolescent.asm"
!src "xchar.asm"
!src "xchar-ext.asm"

INITIAL_FORTH_WORDLIST
        !word _here
INITIAL_HERE

;TOP  :    .END           ; end of listing
;