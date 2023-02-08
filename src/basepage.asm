
        ; our base page stuff
        !align $ff, 0
base_page
!pseudopc $0000 {

; Forth VM registers:
; reg_R !word 0

;      IP       address of the Interpretive Pointer in zero-page.
I               !word 0

DO_JUMP_W
        !byte $6c       ; jmp (W)
;      W        address of the code field pointer in zero-page.
W               !word 0

;      UP       address of the User Pointer in zero-page.
U               !word 0
; floating point stack?

; X holds the stack pointer (S)
;      XSAVE    address of a temporary register for X in zero-page.
XSAVE       !byte 0 ; temporary

; TODO input buffer stack (need to support depth of 8)
; TODO open file info
SOURCE_ID   !word 0

WORDP       !word 0 ; temporary pointer to iterate over words TODO REMOVE
STRING      !word 0 ; pointer to string to print, etc TODO REMOVE

SCREEN_X    !byte 0
SCREEN_Y    !byte 0

!ifdef DEBUG {
;XW        =$12           ; scratch reg. to next code field add
;NP        =$14           ; scratch reg. pointing to name field
}

; TODO make these 32-bit (or at least 28)
SCREEN_LINE !32 0 ; start of line at SCREEN_Y
COLOUR_LINE !32 0 ; start of colour at SCREEN_Y
COLOUR          !byte 0
        ; !word 0

; BOS       = $20                         ; bottom of data stack, in zero-page.
; TOS       = $9E                         ; top of data stack, in zero-page.

BOS = * ; Bottom of data stack

        ; stack
        !align $ff, $9E

TOS = * ; Top of data stack
; N         = TOS+8                       ; scratch workspace.
; IP        = N+8                         ; interpretive pointer.
; W         = IP+3                        ; code field pointer.
; UP        = W+2                         ; user area pointer.
; XSAVE     = UP+2                        ; temporary for X register.

        ; TODO stacks?
}
        ; !align 255, 0

