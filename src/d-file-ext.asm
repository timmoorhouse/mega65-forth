
; ****************************************************************************
; FILE EXT

; ****************************************************************************
; FILE-STATUS
; (c-addr u -- x ior)

!if 0 {
!if ENABLE_FILE_EXT {
        +CREATE "file-status", 0
W_FILE_STATUS
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO

        !word W_PSEMI
}
}

; ****************************************************************************
; RENAME-FILE
; (c-addr_1 u_1 c-addr_2 u_2 -- ior)

!if 0 {
!if ENABLE_FILE_EXT {
        +CREATE "rename-file", 0
W_RENAME_FILE
        !word DO_COLON

        ; TODO
        !word W_2DROP        
        !word W_2DROP
        !word W_ZERO    

        !word W_PSEMI    
}
}

; ****************************************************************************
; REQUIRED
; (i*x c-addr u -- i*x)

; See reference implementation

!if 0 {
!if ENABLE_FILE_EXT {
        +CREATE "required", 0
W_REQUIRED
        !word DO_COLON

        ; TODO
        !word W_2DROP 

        !word W_PSEMI    
}
}
