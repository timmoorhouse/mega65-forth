
; ****************************************************************************
; FILE EXT

; ****************************************************************************
; FILE-STATUS
; (c-addr u -- x ior)
; ANSI 11.6.2.1524

!if ENABLE_FILE_EXT {
        +WORD "file-status", 0
W_FILE_STATUS
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO

        !word W_PSEMI
}

; ****************************************************************************
; RENAME-FILE
; (c-addr_1 u_1 c-addr_2 u_2 -- ior)
; ANSI 11.6.2.2130

!if ENABLE_FILE_EXT {
        +WORD "rename-file", 0
W_RENAME_FILE
        !word DO_COLON

        ; TODO
        !word W_2DROP        
        !word W_2DROP
        !word W_ZERO    

        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRED
; (i*x c-addr u -- i*x)
; Forth 2012 11.6.2.2144.50

; See reference implementation

!if ENABLE_FILE_EXT {
        +WORD "required", 0
W_REQUIRED
        !word DO_COLON

        ; TODO
        !word W_2DROP 

        !word W_PSEMI    
}
