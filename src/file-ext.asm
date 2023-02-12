
; ****************************************************************************
; FILE EXT

; ****************************************************************************
; FILE-STATUS
; (c-addr u -- x ior)
; ANSI 11.6.2.1524

!if ENABLE_FILE {
}

; ****************************************************************************
; FLUSH-FILE
; (fileid -- ior)
; ANSI 11.6.2.1560

!if ENABLE_FILE {
}

; ****************************************************************************
; INCLUDE
; Forth 2012 11.6.2.1714

!if ENABLE_FILE {
}

; ****************************************************************************
; REFILL
; (-- flag)
; ANSI 7.6.2.2125
; ANSI 11.6.2.2125

!if ENABLE_FILE {
        +WORD "refill"
W_REFILL
        !word DO_COLON
        !word W_PSEMI    
}

; ****************************************************************************
; RENAME-FILE
; (c-addr_1 u_1 c-addr_2 u_2 -- ior)
; ANSI 11.6.2.2130

!if ENABLE_FILE {
        +WORD "rename-file"
W_RENAME_FILE
        !word DO_COLON
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRE
; Forth 2012 11.6.2.2144.10

!if ENABLE_FILE {
        +WORD "require"
W_REQUIRE
        !word DO_COLON
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRED
; Forth 2012 11.6.2.2144.50

!if ENABLE_FILE {
        +WORD "required"
W_REQUIRED
        !word DO_COLON
        !word W_PSEMI    
}

; ****************************************************************************
; S\"
; Forth 2012 11.6.2.2266

; See core-ext
