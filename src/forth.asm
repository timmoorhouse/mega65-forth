
!cpu m65
!convtab pet

; TODO clean up the symbol names - what does acme allow?

; TODO remove these ENABLE flags

!ifndef ENABLE_BLOCK                    { ENABLE_BLOCK                  = 0 }
!ifndef ENABLE_BLOCK_EXT                { ENABLE_BLOCK_EXT              = 0 }
!if 1                                   { ENABLE_CORE                   = 1 } ; Required
!ifndef ENABLE_CORE_EXT                 { ENABLE_CORE_EXT               = 1 }
!ifndef ENABLE_CORE_EXT_OBSOLESCENT     { ENABLE_CORE_EXT_OBSOLESCENT   = 0 }
!ifndef ENABLE_DOUBLE                   { ENABLE_DOUBLE                 = 1 }
!ifndef ENABLE_DOUBLE_EXT               { ENABLE_DOUBLE_EXT             = 1 }
!ifndef ENABLE_EXCEPTION                { ENABLE_EXCEPTION              = 1 }
!ifndef ENABLE_EXCEPTION_EXT            { ENABLE_EXCEPTION_EXT          = 1 }
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
!ifndef CASE_INSENSITIVE                { CASE_INSENSITIVE              = 1 } ; map names to lower case when defining/resolving
!ifndef NICE_ERROR_MESSAGES             { NICE_ERROR_MESSAGES           = 1 }
!ifndef EMBED_BOOTSTRAP_MIN             { EMBED_BOOTSTRAP_MIN           = 1 }

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

THEME_OUTPUT = 0
THEME_INPUT  = 1
THEME_PROMPT = 2
THEME_ERROR  = 3

;
; TODO does it make sense to use the basic rom at all? I'm wondering about math routines in particular, but there
; may not be a good way to use them (no jump vectors to them so they could move) - might be able to execute a token for them

; TODO inw/dew to increment/decrement words!
; TODO inq/deq for quads !!!
; TODO rolq, rorq, aslq
; TODO asw for asl on a word!
; TODO row for rol on a word!
; TODO neg for twos complement
; TODO phw to push a word? only out of memory (maybe not so useful) or immediate (could be useful)
; TODO quad stuff for double precision things
; TODO bit supports $nn,x

!source "util.asm"
!source "dma.asm"
!source "vic4.asm"
!source "gpio.asm"

; [-255,-1] are reserved for use by the standard
E_ABORT                          =  -1 ; ABORT
E_ABORTQ                         =  -2 ; ABORT"
E_DATA_STACK_OVERFLOW            =  -3 ; stack overflow
E_DATA_STACK_UNDERFLOW           =  -4 ; stack underflow
E_RETURN_STACK_OVERFLOW          =  -5 ; return stack overflow
E_RETURN_STACK_UNDERFLOW         =  -6 ; return stack underflow
E_LOOP_NESTING_TOO_DEEP          =  -7 ; do loops nested too deeply during execution
E_DICTIONARY_OVERFLOW            =  -8 ; dictionary overflow
E_INVALID_ADDRESS                =  -9 ; invalid memory address
E_DIVIDE_BY_ZERO                 = -10 ; division by zero
E_OUT_OF_RANGE                   = -11 ; result out of range
E_TYPE_MISMATCH                  = -12 ; argument type mismatch
E_UNDEFINED_WORD                 = -13 ; undefined word
E_INTERPRET_COMPILE_ONLY         = -14 ; interpreting a compile-only word
E_INVALID_FORGET                 = -15 ; invalid FORGET
E_ZERO_LENGTH_NAME               = -16 ; attempt to use zero length string as a name
E_PICTURED_NUMERIC_OVERFLOW      = -17 ; pictured numeric output string overflow
E_PARSED_STRING_OVERFLOW         = -18 ; parsed string overflow
E_NAME_TOO_LONG                  = -19 ; definition name too long
E_WRITE_TO_READ_ONLY             = -20 ; write to a read-only location
E_UNSUPPORTED_OPERATION          = -21 ; unsupported operation (eg AT-XY on a too-dumb terminal)
E_CONTROL_STRUCTURE_MISMATCH     = -22 ; control structure mismatch
E_ADDRESS_ALIGNMENT              = -23 ; address alignment exception
E_INVALID_NUMERIC_ARGUMENT       = -24 ; invalid numeric argument
E_RETURN_STACK_IMBALANCE         = -25 ; return stack imbalance
E_LOOP_PARAMETERS_UNAVAILABLE    = -26 ; loop parameters unavailable
E_INVALID_RECURSION              = -27 ; invalid recursion
E_USER_INTERRUPT                 = -28 ; user interrupt
E_COMPILER_NESTING               = -29 ; compiler nesting
E_OSBSOLESCENT_FEATURE           = -30 ; obsolescent feature
; E_                               = -31 ; >BODY used on non-CREATEd definition
E_INVALID_NAME                   = -32 ; invalid name argument (eg TO name)
E_BLOCK_READ                     = -33 ; block read exception
E_BLOCK_WRITE                    = -34 ; block write exception
E_INVALID_BLOCK                  = -35 ; invalid block number
E_INVALID_FILE_POSITION          = -36 ; invalid file position
E_FILE_IO                        = -37 ; file I/O exception
E_NONEXISTENT_FILE               = -38 ; non-existent file
E_UNEXPECTED_EOF                 = -39 ; unexpected end of file
E_FP_INVALID_BASE                = -40 ; invalid BASE for floating point conversion
E_LOSS_OF_PRECISION              = -41 ; loss of precision
E_FP_DIVIDE_BY_ZERO              = -42 ; floating point divide by zero
E_FP_OUT_OF_RANGE                = -43 ; floating point result out of range
E_FP_STACK_OVERFLOW              = -44 ; floating point stack overflow
E_FP_STACK_UNDERFLOW             = -45 ; floating point stack underflow
E_FP_INVALID_ARGUMENT            = -46 ; floating point invalid argument
E_COMPILATION_WORDLIST_DELETED   = -47 ; compilation word list deleted
E_INVALID_POSTPONE               = -48 ; invalid POSTPONE
E_SEARCH_ORDER_OVERFLOW          = -49 ; search order overflow
E_SEARCH_ORDER_UNDERFLOW         = -50 ; search order underflow
E_COMPILATION_WORDLIST_CHANGED   = -51 ; compilation word list changed
E_CONTROL_FLOW_STACK_OVERFLOW    = -52 ; control flow stack overflow
E_EXCEPTION_STACK_OVERFLOW       = -53 ; exception stack overflow
E_FP_UNDERFLOW                   = -54 ; floating point underflow
E_FP_UNIDENTIFIED_FAULT          = -55 ; floating point unidentified fault
E_QUIT                           = -56 ; QUIT
; E_                               = -57 ; exception in sending or receiving a character
; E_                               = -58 ; [IF], [ELSE] or [THEN] exception
E_ALLOCATE                       = -59 ; ALLOCATE
E_FREE                           = -60 ; FREE
E_RESIZE                         = -61 ; RESIZE
E_CLOSE_FILE                     = -62 ; CLOSE-FILE
E_CREATE_FILE                    = -63 ; CREATE-FILE
E_DELETE_FILE                    = -64 ; DELETE-FILE
E_FILE_POSITION                  = -65 ; FILE-POSITION
E_FILE_SIZE                      = -66 ; FILE-SIZE
E_FILE_STATUS                    = -67 ; FILE-STATUS
E_FLUSH_FILE                     = -68 ; FLUSH-FILE
E_OPEN_FILE                      = -69 ; OPEN-FILE
E_READ_FILE                      = -70 ; READ-FILE
E_READ_LINE                      = -71 ; READ-LINE
E_RENAME_FILE                    = -72 ; RENAME-FILE
E_REPOSITION_FILE                = -73 ; REPOSITION-FILE
E_RESIZE_FILE                    = -74 ; RESIZE-FILE
E_WRITE_FILE                     = -75 ; WRITE-FILE
E_WRITE_LINE                     = -76 ; WRITE-LINE
E_MALFORMED_XCHAR                = -77 ; malformed xchar
E_SUBSTITUTE                     = -78 ; SUBSTITUTE
E_REPLACES                       = -79 ; REPLACES

; [-4095,-256] are reserved for the implementation
E_WORDLIST_NOT_AVAILABLE         = -256
;
; Errors to create:
;
;

* = $2001
        +upstart entry
entry
        jmp COLD

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
;      |               Data stack 
;      |                    |
;      |                    V
;      |
;      | Basepage data - W, I, etc
;      +-----------------------------    <--- BASEPAGE
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
;          gap for HOLD and WORD transient areas
;      +-----------------------------    <--- HERE
;      |
;      | Dictionary
;      |
;      | predefined words
;      |
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
BASEPAGE           = LIMIT - $100
TIB_LEN            = 80
TIB                = BASEPAGE - TIB_LEN
NUM_STRING_BUFFERS = 2
STRING_BUFFER_LEN  = 80
SBUF_LEN           = NUM_STRING_BUFFERS * STRING_BUFFER_LEN
SBUF               = TIB - SBUF_LEN
DAREA_LEN          = MAX_OPEN_FILES * FILE_BUFFER_SIZE 
DAREA              = SBUF - DAREA_LEN
HOLD_LEN           = 34 ; min (2*16)+2 = 34
WORD_LEN           = 33 ; min 33
PAD_LEN            = 84

pre_basepage = *
!pseudopc BASEPAGE {
!source "basepage.asm"
}
* = pre_basepage, overlay

; TODO transient buffer for s", s\" (need 2 buffers so that two consecutive
; strings can be stored)
; so the following should work:
;     s" abc" s" def" rename-file

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
;      | Flags | name length (5 bits)
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

!macro NONAME {
        +ALIGN
}

!set _here = $0

; Control bits
F_IMMEDIATE    = $80
F_COMPILE_ONLY = $40
; <unused>       $20 ; TODO
NAME_LEN_MASK  = $1f ; use lower bits for name length

!macro WORD .name, .flags {
        +ALIGN
        !word _here
        !set _here = *-2
        !byte len(.name) | .flags
        !text .name
        +ALIGN
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

!macro CSLITERAL .text {
        !word W_PCSLITERAL
        +STRING .text
}

!macro DOTQ .text {
        +CSLITERAL .text
        !word W_COUNT
        !word W_TYPE
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
!if ENABLE_RUNTIME_CHECKS {
        ; TODO check for underflow here (would need to check that words aren't bypassing this)
}
        bra NEXT

        ; A + top of stack is the 16-bit value
PUSH
        dex
        dex
        ; jmp PUT
!if ENABLE_RUNTIME_CHECKS {
        ; TODO check for overflow here (would need to check that words aren't bypassing this)
}

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

        ; Zero fill basepage
        +dma_inline
        !byte $0a                       ; F018A 11-byte format
        +dma_options_end
        !byte dma_cmd_fill
        !word $100
        !word 0                         ; src (fill value in LSB)
        !byte 0                         ; src bank/flags
        !word BASEPAGE
        !byte 0                         ; dst bank/flags
        !word 0                         ; modulo

        lda #$6c                        ; jmp (W)
        sta &DO_JUMP_W

        lda #10
        sta &BASE
    
JSR_ONETIME
        jsr _onetime            ; will be replaced with NOPs in _onetime
    
WARM

        ; Save return stack pointer in R0
        jsr RPAT
        sta <R0
        sty <R0+1

        ; Reposition data stack
        ldx #TOS-2 ; TODO remove the -2?

        ; Save data stack pointer in S0
        stx <S0
        ldy #0
        sty <S0+1

        ; ldy #0 ; still 0 from above
        sty <SOURCE_ID          ; TODO do we need this?
        sty <SOURCE_ID+1

        cld
        see

        lda #<W_MAIN+2
        sta <I
        lda #>W_MAIN+2
        sta <I+1

        lda #14                 ; Lower case
        jsr EMIT
        lda #11                 ; Disable shift-mega case changes TODO skip this?
        jsr EMIT
!if 1 {                         ; TODO this seems to cause problems on a real MEGA65
        lda #27                 ; ESC 5 - switch to 80x50
        jsr EMIT
        lda #53
        jsr EMIT
}

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

        ; TODO report bytes free

        ; just in case the colour scheme is disabled ...
        lda #1 ; white
        jsr FOREGROUND

        jmp NEXT

_startup_text1
        +STRING "MEGA65-Forth "
_startup_text2
        +STRING "bye will exit to BASIC\r"
!ifdef HAVE_REVISION {
!src "revision.asm"
}

        +WORD "autoboot", 0
W_AUTOBOOT
        !word DO_DEFER
        !word W_AUTOBOOT_BOOTSTRAP

        +WORD "(quit)", 0
W_PQUIT
        !word DO_DEFER
        !word W_DEFER_UNINITIALIZED

        +NONAME
W_MAIN
        !word DO_COLON

        +LITERAL W_AUTOBOOT
        !word W_CATCH
        !word W_QDUP
        +ZBRANCH +
        !word W_EDOT
+

_main_clear_stack_and_enter_loop

        +LITERAL &S0
        !word W_AT
        !word W_SPSTORE

_main_loop

        ; TODO resolve this using find-name or defer it?
        +LITERAL W_PQUIT
        !word W_CATCH

!if 0 {
        !word W_DOTS
        !word W_CR
}

        !word W_QDUP
        +ZBRANCH _main_loop

        !word W_DUP
        +LITERAL E_ABORT
        !word W_EQUAL
        +ZBRANCH +

        !word W_DROP

        +BRANCH _main_clear_stack_and_enter_loop

+

        !word W_DUP
        +LITERAL E_ABORTQ
        !word W_EQUAL
        +ZBRANCH +

        !word W_EDOT

        +BRANCH _main_clear_stack_and_enter_loop

+

        !word W_DUP
        +LITERAL E_QUIT
        !word W_EQUAL
        +ZBRANCH +

        !word W_DROP

        +BRANCH _main_loop

+

        !word W_EDOT

        +BRANCH _main_loop
        ; +BRANCH _main_clear_stack_and_enter_loop

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
!src "tools.asm"
!src "tools-ext.asm"
!src "tools-ext-obsolescent.asm"
!src "xchar.asm"
!src "xchar-ext.asm"


        +WORD "here", 0
W_HERE
        !word DO_CONSTANT
HERE
        !word 0

INITIAL_HERE

;
; One-time initialization code that is safe to discard
; before bootstrapping begins.
;

_onetime

        ; Replace the 'jsr _onetime' with CLDs so this won't be done again
        lda #$d8                        ; CLD
        sta JSR_ONETIME
        sta JSR_ONETIME+1
        sta JSR_ONETIME+2

        ; TODO clean this up
        lda #<_here
        sta FORTH_WORDLIST
        lda #>_here
        sta FORTH_WORDLIST+1

        lda #<INITIAL_HERE
        sta HERE
        lda #>INITIAL_HERE
        sta HERE+1

        rts

;
; The bootstrapping code proper is placed higher in memory.
; It must be preserved for the duration of the bootstrapping
; process (it is placed higher in memory to keep it from being
; overwritten by the expanding dictionary), but can be discarded
; once bootstrapping is complete.
;

* = $8000
!src "bootstrap.asm"

; TODO report collision

;!if * > DAREA {
;        !error "embedded bootstrap code colliding with high memory"
;} else {

; This gap must be >= 0
!warn DAREA - *, " byte gap between bootstrap code and high memory"

;}
