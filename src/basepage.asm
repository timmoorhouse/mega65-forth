
                ; our base page stuff
;                !align $ff, 0
base_page
!pseudopc $0000 {

; Forth VM registers:
; reg_R !word 0

;      IP       address of the Interpretive Pointer in zero-page.
I               !word 0

; TODO alignment
DO_JUMP_W
                !byte 0       ; set to $6c jmp (W) during _onetime
;      W        address of the code field pointer in zero-page.
W               !word 0

                !byte 0         ; alignment

; floating point stack?

; TODO use a word for this? (turn it into S)
; X holds the stack pointer (S)
;      XSAVE    address of a temporary register for X in zero-page.
XSAVE           !byte 0 ; temporary to save S when we need to reuse X
; TODO separate XSAVE for kernel calls
KERNEL_XSAVE    !byte 0 ; just for use in kernel calls

; TODO move these out of basepage? make them values ...
INPUT_BUFFER    !word 0 ; Address and length of input buffer
INPUT_LEN       !word 0

BASE            !word 0 ; TODO MOVE BELOW HERE?
STATE           !word 0
R0              !word 0
S0              !word 0

TEMP1           !word 0 ; temporaries
TEMP2           !word 0
TEMP3           !word 0

STRING          !word 0 ; pointer to string to print, etc TODO REMOVE

BOS = * ; Bottom of data stack

                ; stack
                ; !align $ff, $9E
                !align $ff, $fe

TOS = * ; Top of data stack
        ;!word 0

}
        ; !align 255, 0

