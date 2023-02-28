
!cpu m65
!convtab pet

; TODO clean up the symbol names - what does acme allow?

!ifndef ENABLE_BLOCK                    { ENABLE_BLOCK                  = 0 }
!ifndef ENABLE_BLOCK_EXT                { ENABLE_BLOCK_EXT              = 0 }
!if 1                                   { ENABLE_CORE                   = 1 } ; Required
!ifndef ENABLE_CORE_EXT                 { ENABLE_CORE_EXT               = 1 }
!ifndef ENABLE_CORE_EXT_OBSOLESCENT     { ENABLE_CORE_EXT_OBSOLESCENT   = 0 }
!ifndef ENABLE_DOUBLE                   { ENABLE_DOUBLE                 = 1 }
!ifndef ENABLE_DOUBLE_EXT               { ENABLE_DOUBLE_EXT             = 1 }
!ifndef ENABLE_EXCEPTION                { ENABLE_EXCEPTION              = 0 }
!ifndef ENABLE_EXCEPTION_EXT            { ENABLE_EXCEPTION_EXT          = 0 }
!ifndef ENABLE_FACILITY                 { ENABLE_FACILITY               = 1 }
!ifndef ENABLE_FACILITY_EXT             { ENABLE_FACILITY_EXT           = 1 }
!ifndef ENABLE_FIG                      { ENABLE_FIG                    = 1 } ; TODO remove
!ifndef ENABLE_FILE                     { ENABLE_FILE                   = 1 }
!ifndef ENABLE_FILE_EXT                 { ENABLE_FILE_EXT               = 1 }
!ifndef ENABLE_FLOATING                 { ENABLE_FLOATING               = 0 }
!ifndef ENABLE_FLOATING_EXT             { ENABLE_FLOATING_EXT           = 0 }
!ifndef ENABLE_GFORTH                   { ENABLE_GFORTH                 = 1 } ; useful things following gforth's extensions
!ifndef ENABLE_LOCALS                   { ENABLE_LOCALS                 = 0 }
!ifndef ENABLE_LOCALS_EXT               { ENABLE_LOCALS_EXT             = 0 }
!ifndef ENABLE_LOCALS_EXT_OBSOLESCENT   { ENABLE_LOCALS_EXT_OBSOLESCENT = 0 }
!ifndef ENABLE_MEGA65                   { ENABLE_MEGA65                 = 1 }
!ifndef ENABLE_MEMORY                   { ENABLE_MEMORY                 = 0 }
!ifndef ENABLE_MEMORY_EXT               { ENABLE_MEMORY_EXT             = 0 }
!ifndef ENABLE_SEARCH                   { ENABLE_SEARCH                 = 1 }
!ifndef ENABLE_SEARCH_EXT               { ENABLE_SEARCH_EXT             = 1 }
!ifndef ENABLE_STRING                   { ENABLE_STRING                 = 1 }
!ifndef ENABLE_STRING_EXT               { ENABLE_STRING_EXT             = 1 }
!ifndef ENABLE_TOOLS                    { ENABLE_TOOLS                  = 1 } ; for words, .s, etc
!ifndef ENABLE_TOOLS_EXT                { ENABLE_TOOLS_EXT              = 1 }
!ifndef ENABLE_TOOLS_EXT_OBSOLESCENT    { ENABLE_TOOLS_EXT_OBSOLESCENT  = 0 }
!ifndef ENABLE_XCHAR                    { ENABLE_XCHAR                  = 0 }
!ifndef ENABLE_XCHAR_EXT                { ENABLE_XCHAR_EXT              = 0 }

!ifndef DEBUG                           { DEBUG                         = 0 }
!ifndef ENABLE_RUNTIME_CHECKS           { ENABLE_RUNTIME_CHECKS         = 0 } ; TODO lots of things are triggering this - need to clean them up before enabling
!ifndef USE_BASIC                       { USE_BASIC                     = 0 } ; not used - REMOVE?
!ifndef CASE_INSENSITIVE                { CASE_INSENSITIVE              = 1 } ; map names to lower case when defining/resolving
!ifndef AUTOBOOT                        { AUTOBOOT                      = 1 } ; Attempt to include autoboot.f on startup

; Runtime checks
; - For anything taking an aligned address, check that it's aligned
;   - !, +!, @, EXECUTE
;   - TODO: MOVE, others?
;   - TODO: , currently uses ! so we'd need to keep HERE aligned (affects CLITERAL, .")
; - TODO: check data stack depth at start of : matches that at start of ; (FIG uses !csp, ?csp for this)
; - TODO: check for data stack overflow/underflow (FIG uses ?STACK)
; - TODO: check for return stack overflow/underflow
; - TODO: check if HERE is too close to DAREA (check in ALLOT)
; - TODO: check if HERE is moved down below its initial value
; - TODO: check value passed to FORGET (FIG uses FENCE for lower limit)

;
; Colour scheme
; These are indexes into the colour pallette (see docs for BACKGROUND for a table)
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
; TODO neg for twos complement
; TODO phw to push a word? only out of memory (maybe not so useful) or immediate (could be useful)
; TODO quad stuff for double precision things

; TODO when pushing words onto the return stack, we've been pushing MSB then LSB (so the result will have LSB first in memory)
; BUT it looks like phw pushes the LSB first then the MSB.  Should we switch the order we use the return stack in?
; This would mean we'd be free to use phw, but it would also mean we couldn't use words in place as they sit on the
; return stack.  Should the data stack follow the same convention?  It seems more problematic to do this for the data stack.
;
; HOWEVER jsr pushes the MSB of the PC first????

PUSH_MSB_FIRST = 1 ; Enabled is the current expectation

!source "util.asm"
!source "dma.asm"
!source "vic4.asm"
!source "gpio.asm"

* = $2001
        +upstart entry
entry
        jmp COLD

; TODO a bunch of wasted space here
!source "basepage.asm"

; MEMORY MAP
; THIS IS STILL EVOLVING
; 
; 
; FFFF +-----------------------------
;      | Kernel
; E000 +----------------------------- 
;      | I/O
; D000 +-----------------------------
;      | Interface 
; C000 +-----------------------------    <--- LIMIT
;      | TODO move basepage here?
;      +-----------------------------
;      | Terminal input buffer
;      +-----------------------------    <--- TIB
;      | String buffers for S", S\"
;      +-----------------------------    <--- SBUF
;      | File buffers
;      | (put terminal buffer here)
;      +-----------------------------    <--- DAREA
; 
; 
;      |
;      | Transient workspace
;      +-----------------------------    <--- PAD 
;          gap?  right now we need this since HOLD works backwards from PAD
;      +-----------------------------    <--- HERE
;      |
;      | Dictionary
;      |
;      | predefined words
; 2200 +-----------------------------
;      |               Data stack       TODO move this region
;      |                    |
;      |                    V
;      |
;      | Basepage data - W, I, etc
; 2100 +-----------------------------
;      | some unused space here
;      | basic stub
; 2001 +-----------------------------
;      | likely some space we could use for small things
;      |
;      |
; 0200 +----------------------------
;      |  return stack                   
;      |       |          
;      |       V           
;      |                   
;      |           
; 0100 +-----------------------------
;      | basic/kernel stuff
; 0000 +-----------------------------

LIMIT              = $C000 ; TODO
TIB_LEN            = 80
TIB                = LIMIT - TIB_LEN
NUM_STRING_BUFFERS = 2
STRING_BUFFER_LEN  = 80
SBUF_LEN           = NUM_STRING_BUFFERS * STRING_BUFFER_LEN
SBUF               = TIB - SBUF_LEN
DAREA_LEN          = MAX_OPEN_FILES * FILE_BUFFER_SIZE 
DAREA              = SBUF - DAREA_LEN

; TODO transient buffer for s", s\" (need 2 buffers so that two consecutive
; strings can be stored)
; so the following should work:
;     s" abc" s" def" rename-file
; TODO can we use PAD and one additional buffer?

; VM Registers
; S - data stack pointer
; R - return stack pointer
; I - instruction pointer
; W - word pointer (to definition currently executing, used to get parameter field)
;
; floating point stack is allowed to be on the data stack or separate
; (might want to look at using the math register area directly?)

; TODO should we have X point to the byte below the type entry on the data stack 
; (like how the hardware return stack pointer behaves)?  It might let us move the
; data stack right up to the end of a page.  We'd need to adjust SP@.

!macro STRING .text {
        !byte len(.text)
        !text .text
}

!macro ALIGN {
  !if *&1 {
        !byte 0
  }
}

; DICTIONARY ENTRY
;
;      +---------------     <--- preceeding link points here - aligned
;      | Link (2 bytes)  
;      |
;      |
;      +-------+-------
;      | Flags | name length (5 bits)      this is still present for unnamed words?
;      +-------+-------
;      | Name (0-31 bytes)
;      | 
;      |
;      +---------------     <--- aligned (if name is even length there will be a pad byte)
;      | code field (2 bytes) DO_COLON, DO_VARIABLE, DO_CONSTANT, *+2, etc
;      +---------------
;      | data field
;      |
;      |
;      |
;      |
;      +---------------

; TODO add a bank field?  It might make sense to keep the wordlists in bank 0 but allow the
; data fields to be in other banks.  We'd need to allow code fields to reference things in other
; banks too (to support does>).

; FIG puts the name (and the associated length/flags) before the link, we put it after
; This makes it a bit tougher to get to the flags, but easier to get to the code field

; TODO we might want a level of indirection for SYNONYM?  We may want the same
; thing for DEFER (though those could be handled with a separate DO_DEFER)

; This is mostly here so I can keep them straight:
; Name token (nt)
;   - TRAVERSE-WORD list passes these to the supplied word when iterating
;   - We use the address of the link field as the name token
; Execution token (xt)
;   - These are the end result of SEARCH-WORDLIST and can be passed to EXECUTE
;   - NAME>INTERPRET will convert from a name token to an execution token
;   - We use the address of the data field for the execution token
; Compilation token (ct)
;   - This is a pair of cells w xt
;   - The execution token xt consumes w and performs the compilation semantics of the 
;     corresponding word (in our case xt will be either EXECUTE or COMPILE, and w
;     will be the execution token of the word)
;   - NAME>COMPILE will convert from a name token to a compilation token
;
; SEACH-WORDLIST gives you a flag for immediate/non-immediate instead.  We implment a SEARCH-WORDLIST-NT
; that provides 0 | nt.

!macro NONAME {
        +ALIGN
        ; TODO should we have a byte for flags (+ len 0 name) then a pad byte for alignment?
        ; this would ensure we'd have flags before every word
}

!set _here = $0

; Control bits:
; - fig always sets bit 8 (so we can find the start of the name crawling back from the code field)
; - precedence (fig uses bit 7 for immediate)
; - smudge (fig uses bit 6 for hidden)
; - length uses bits 1-5

; TODO can we get rid of the hidden flag by just not linking the word until ; ?

F_IMMEDIATE   = $80
F_HIDDEN      = $40 ; TODO remove this
; <unused>      $20 ; TODO add a compile-only flag?
NAME_LEN_MASK = $1f

!macro WORD2 .name, .flags {
        +ALIGN
        !word _here
        !set _here = *-2
        !byte len(.name) | .flags
        !text .name
        +ALIGN
}

!macro WORD .name {
        +WORD2 .name, 0
}

!macro WORD_IMM .name {
        +WORD2 .name, F_IMMEDIATE
}

!macro BRANCH .target {
        !word W_BRANCH
        !word .target-*
}

!macro ZBRANCH .target {
        !word W_ZBRANCH
        !word .target-*
}

!macro DO .end {
        !word W_PDO
        !word .end
}

!macro LITERAL .word {
        !word W_PLITERAL
        !word .word
}

!macro CLITERAL .char {
        !word W_PCLITERAL
        !byte .char
}

!macro DOTQ .text {    ; TODO CLEAN THIS UP
        !word W_PDOTQ
        +STRING .text
}

!ifdef DEBUG                { !src "debug.asm"         }

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
        bra NEXT

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

        inw <I           ; Increment I by two
        inw <I

!if DEBUG {
        lda <W+1
        jsr put_hex
        lda <W
        jsr put_hex
        lda #' '
        jsr EMIT
        ldy #0
}
        ; After the jmp:
        ; - X contains the data stack pointer (this should always be preserved)
        ; - Y contains 0 (this can be trashed) TODO should we depend on this?
        ; - A & Z contain nothing in particular (and can be trashed) TODO can we use z for anything?
        ; - TODO can we assume anything about flags?
        jmp &DO_JUMP_W

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

        +map_reset

        ; E000-FFFF     3E000   KERNAL
        ; C000-DFFF
        ; A000-BFFF
        ; 8000-9FFF
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

        ; TODO redirect monexit so we can return from monitor back into forth

        ; set our base page
        lda #>base_page
        tab
        
WARM

        ; Save return stack pointer in R0
        jsr RPAT
        sta <R0
        sty <R0+1

        ; Reposition data stack
        ldx #TOS

        ; Save data stack pointer in S0
        stx <S0
        ldy #0
        sty <S0+1

        lda INITIAL_FORTH_WORDLIST
        sta FORTH_WORDLIST
        lda INITIAL_FORTH_WORDLIST+1
        sta FORTH_WORDLIST+1

        lda #<INITIAL_HERE
        sta <HERE
        lda #>INITIAL_HERE
        sta <HERE+1

        ; ldy #0 ; still 0 from above
        sty <SOURCE_ID
        sty <SOURCE_ID+1

        cld

!if AUTOBOOT {
        W_STARTUP = W_AUTOBOOT
} else {
        W_STARTUP = W_ABORT
}

        lda #<W_STARTUP+2
        sta <I
        lda #>W_STARTUP+2
        sta <I+1

        lda #14                 ; Lower case
        jsr EMIT
        lda #11                 ; Disable shift-mega case changes TODO skip this?
        jsr EMIT
        lda #27                 ; ESC 5 - switch to 80x50
        jsr EMIT
        lda #53
        jsr EMIT

        ; TODO a FONT word that switches font
        +dma_inline             ; copy from $0029000 to $ff7e000
        !byte $0a               ; F018A 11-byte format  
        !byte $81, $ff          ; dst MB      
        +dma_options_end
        !byte dma_cmd_copy
        !word $1000             ; count
        !word $9000             ; src
        !byte $02               ; src bank/flags
        !word $e000             ; dst
        !byte $07               ; dst bank/flags
        !word 0                 ; modulo
; A: 029000
; B: 03D000
; C: 02D000

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

        jmp NEXT

_startup_text1
        +STRING "MEGA65-Forth 0.1"
_startup_text2
        +STRING "bye will exit to BASIC\r"
!ifdef HAVE_REVISION {
!src "revision.asm"
}


!if AUTOBOOT {
        +NONAME
W_AUTOBOOT
        !word DO_COLON
        !word W_DECIMAL ; abort will do this but we need it before autobooting
        +LITERAL AUTOBOOT_FILENAME
        !word W_COUNT
        !word W_INCLUDED
        !word W_ABORT

AUTOBOOT_FILENAME
        +STRING "autoboot.f"
}

!src "block.asm"
!src "block-ext.asm"
!src "core.asm"
!src "core-ext.asm"
!src "core-ext-obsolescent.asm"
!src "double.asm"
!src "double-ext.asm"
!src "exception.asm"
; !src "exception-ext.asm"              ; TODO no need for one yet
!src "facility.asm"
!src "facility-ext.asm"
!src "fig.asm"
!src "file.asm"
!src "file-ext.asm"
!src "floating.asm"
!src "floating-ext.asm"
!src "gforth.asm"
!src "locals.asm"
!src "locals-ext.asm"
!src "locals-ext-obsolescent.asm"
!src "mega65.asm"
!src "memory.asm"
; !src "memory-ext.asm"                 ; TODO no need for one yet
!src "search.asm"
!src "search-ext.asm"
!src "string.asm"
!src "string-ext.asm"
; !src "tools.asm"                      ; TODO no need for one yet
!src "tools-ext.asm"
!src "tools-ext-obsolescent.asm"
!src "xchar.asm"
!src "xchar-ext.asm"

INITIAL_FORTH_WORDLIST
        !word _here ; TODO can we get away without storing this?
INITIAL_HERE
