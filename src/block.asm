 
; ****************************************************************************
; BLOCK

!if ENABLE_BLOCK {

; ****************************************************************************
; BLK
; (-- a-addr)
; ANSI 7.6.1.0790

; FIG:
;
;      BLK           ---  addr                               U,L0
;               A user variable containing the block number being 
;               interpreted.  If zero, input is being taken from the 
;               terminal input buffer.
;
;;
;;                                       BLK
;;                                       SCREEN 36 LINE 10
;;
        +WORD "blk"
W_BLK
        !word DO_USER
        !byte U_BLK

; ****************************************************************************
; BLOCK
; (u -- a-addr)
; ANSI 7.6.1.0800

; FIG:
;
;      BLOCK         n  ---  addr                            L0
;               Leave the memory address of the block buffer containing 
;               block n.  If the block is not already in memory, it is 
;               transferred from disc to which ever buffer was least 
;               recently written.  If the block occupying that buffer has 
;               been marked as being updated, it is re-written to disc 
;               before block n is read into the buffer.  See also BUFFER, 
;               R/W  UPDATE  FLUSH
;
;;
;;                                       BLOCK
;;                                       SCREEN 60 LINE 1
;;
        +WORD "block"
W_BLOCK
        !word DO_COLON
;          !word OFSET
;          !word AT
;          !word PLUS
;          !word TOR
;          !word PREV
;          !word AT
;          !word DUP
;          !word AT
;          !word R
;          !word SUB
;          !word DUP
;          !word PLUS
;          !word ZBRAN
;L2804:    !word $34      ; L2830-L2804
;L2805:    !word PBUF
;          !word ZEQU
;          !word ZBRAN
;L2808:    !word $14      ; L2818-L2808
;          !word DROP
;          !word R
;          !word BUFFR
;          !word DUP
;          !word R
;          !word ONE
;          !word RSLW
;          !word TWO
;          !word SUB
;L2818:    !word DUP
;          !word AT
;          !word R
;          !word SUB
;          !word DUP
;          !word PLUS
;          !word ZEQU
;          !word ZBRAN
;L2826:    !word $FFD6    ; L2805-L2826
;          !word DUP
;          !word PREV
;          !word STORE
;L2830:    !word RFROM
;          !word DROP
;          !word TWOP
        !word W_SEMI

; ****************************************************************************
; BUFFER
; (u -- a-addr)
; ANSI 7.6.1.0820

; FIG:
;
;      BUFFER        n  ---  addr
;               Obtain the next memory buffer, assigning it to block n.  
;               If the contents of the buffer is marked as updated, it is 
;               written to the disc.  The block is not read from the disc.  
;               The address left is the first cell within the buffer for 
;               data storage.
;
;;
;;                                       BUFFER
;;                                       SCREEN 59 LINE 1
;;
        +WORD "buffer"
W_BUFFER
        !word DO_COLON
;          !word USE
;          !word AT
;          !word DUP
;          !word TOR
;L2758:    !word PBUF
;          !word ZBRAN
;L2760:    !word $FFFC    ; L2758-L2760
;          !word USE
;          !word STORE
;          !word R
;          !word AT
;          !word ZLESS
;          !word ZBRAN
;L2767:    !word $14      ; L2776-L2767
;          !word R
;          !word TWOP
;          !word R
;          !word AT
;          !word LIT,$7FFF
;          !word ANDD
;          !word ZERO
;          !word RSLW
;L2776:    !word R
;          !word STORE
;          !word R
;          !word PREV
;          !word STORE
;          !word RFROM
;          !word TWOP
        !word W_SEMI

; ****************************************************************************
; EVALUATE
; ANSI 7.6.1.1360

; ****************************************************************************
; FLUSH
; (--)
; ANSI 7.6.1.1559

; FIG:
;;
;;                                       FLUSH
;;
        +WORD "flush"
W_FLUSH
        !word DO_COLON
;          !word LIMIT,FIRST,SUB
;          !word BBUF,CLIT
;          !byte 4
;          !word PLUS,SLASH,1PLUS
;          !word ZERO,PDO
;L2835:    !word LIT,$7FFF,BUFFR
;          !word DROP,PLOOP
;L2839:    !word $FFF6    ; L2835-L2839
        !word W_SEMI

; ****************************************************************************
; LOAD
; (i*x u -- j*x)
; ANSI 7.6.1.1790

; FIG:
;      LOAD          n  ---                                  L0
;               Begin interpretation of screen n.  Loading will terminate 
;               at the end of the screen or at ;S.  See ;S and -->.
;
;;
;;                                       LOAD
;;                                       SCREEN 62 LINE 2
;;
        +WORD "load"
W_LOAD
        !word DO_COLON
;          !word BLK
;          !word AT
;          !word TOR
;          !word IN
;          !word AT
;          !word TOR
;          !word ZERO
;          !word IN
;          !word STORE
;          !word BSCR
;          !word STAR
;          !word BLK
;          !word STORE
;          !word INTER
;          !word RFROM
;          !word IN
;          !word STORE
;          !word RFROM
;          !word BLK
;          !word STORE
        !word W_SEMI

; ****************************************************************************
; SAVE-BUFFERS
; (--)
; ANSI 7.6.1.2180

; ****************************************************************************
; UPDATE
; (--)
; ANSI 7.6.1.2400

; FIG:
;      UPDATE                                                L0
;               Marks the most recently referenced block (pointed to by 
;               PREV) as altered.  The block will subsequently be 
;               transferred automatically to disc should its buffer be 
;               required for storage of a different block.
;
;;
;;                                       UPDATE
;;                                       SCREEN 58 LINE 8
;;
        +WORD "update"
W_UPDATE
        !word DO_COLON
;          !word PREV
;          !word AT
;          !word AT
;          !word LIT,$8000
;          !word OR
;          !word PREV
;          !word AT
;          !word STORE
        !word W_SEMI

}
