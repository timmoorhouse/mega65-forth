
                ; our base page stuff
                !align $ff, 0
base_page
!pseudopc $0000 {

; Forth VM registers:
; reg_R !word 0

;      IP       address of the Interpretive Pointer in zero-page.
I               !word 0

; TODO alignment
DO_JUMP_W
                !byte $6c       ; jmp (W)
;      W        address of the code field pointer in zero-page.
W               !word 0

; floating point stack?

; TODO use a word for this? (turn it into S)
; X holds the stack pointer (S)
;      XSAVE    address of a temporary register for X in zero-page.
XSAVE           !byte 0 ; temporary to save S when we need to reuse X
; TODO separate XSAVE for kernel calls
KERNEL_XSAVE    !byte 0 ; just for use in kernel calls

; TODO alignment
; TODO input buffer stack (need to support depth of 8)
; TODO open file info
SOURCE_ID       !word 0
INPUT_BUFFER    !word 0 ; Address and length of input buffer
INPUT_LEN       !word 0
IN              !word 0 ; Offset of start of parse area within input buffer

HERE            !word 0

TEMP1           !word 0 ; temporaries
TEMP2           !word 0
TEMP3           !word 0

WORDP           !word 0 ; temporary pointer to iterate over words TODO REMOVE
STRING          !word 0 ; pointer to string to print, etc TODO REMOVE

BASE            !word 0
STATE           !word 0
R0              !word 0
S0              !word 0

!ifdef DEBUG {
;XW        =$12           ; scratch reg. to next code field add
;NP        =$14           ; scratch reg. pointing to name field
}

; BOS       = $20                         ; bottom of data stack, in zero-page.
; TOS       = $9E                         ; top of data stack, in zero-page.

BOS = * ; Bottom of data stack

                ; stack
                !align $ff, $9E

TOS = * ; Top of data stack

}
        ; !align 255, 0

