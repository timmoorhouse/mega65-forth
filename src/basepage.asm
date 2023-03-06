
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

; floating point stack?

; TODO use a word for this? (turn it into S)
; X holds the stack pointer (S)
;      XSAVE    address of a temporary register for X in zero-page.
XSAVE           !byte 0 ; temporary to save S when we need to reuse X
; TODO separate XSAVE for kernel calls
KERNEL_XSAVE    !byte 0 ; just for use in kernel calls

NEXT_SBUF       !byte 0 ; TODO move out of base page?

; TODO move these out of basepage?
SOURCE_ID       !word 0
INPUT_BUFFER    !word 0 ; Address and length of input buffer
INPUT_LEN       !word 0
IN              !word 0 ; Offset of start of parse area within input buffer

HERE            !word 0

BASE            !word 0
STATE           !word 0
R0              !word 0
S0              !word 0
LATEST          !word 0
LATEST_XT       !word 0

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

