
; ****************************************************************************
; FILE EXT

; ****************************************************************************
; FILE-STATUS
; (c-addr u -- x ior)
; ANSI 11.6.2.1524

!if ENABLE_FILE {
        +WORD "file-status"
W_FILE_STATUS
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<file-status>"
        !word W_DOTS
}       
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; FLUSH-FILE
; (fileid -- ior)
; ANSI 11.6.2.1560

!if ENABLE_FILE {
        +WORD "flush-file"
W_FLUSH_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<flush-file>"
        !word W_DOTS
}       
        ; TODO
        !word W_DROP
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; INCLUDE
; (i*x "name" -- j*x)
; Forth 2012 11.6.2.1714

!if ENABLE_FILE {
        +WORD_IMM "include"
W_INCLUDE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<include>"
        !word W_DOTS
}       
        ; TODO
        !word W_PARSE_NAME
        !word W_2DROP
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI     
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
 !if DEBUG {
        !word W_PDOTQ
        +STRING "<refill>"
        !word W_DOTS
}       
        ; TODO
        !word W_ZERO  
!if DEBUG {
        !word W_DOTS,W_CR
}
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
!if DEBUG {
        !word W_PDOTQ
        +STRING "<rename-file>"
        !word W_DOTS
}       
        ; TODO
        !word W_2DROP        
        !word W_2DROP
        !word W_ZERO    
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRE
; (i*x "name" -- i*x)
; Forth 2012 11.6.2.2144.10

!if ENABLE_FILE {
        +WORD_IMM "require"
W_REQUIRE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<require>"
        !word W_DOTS
}       
        ; TODO
        !word W_PARSE_NAME
        !word W_2DROP    
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRED
; (i*x c-addr u -- i*x)
; Forth 2012 11.6.2.2144.50

!if ENABLE_FILE {
        +WORD_IMM "required"
W_REQUIRED
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<required>"
        !word W_DOTS
}       
        ; TODO
        !word W_2DROP 
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; S\"
; Forth 2012 11.6.2.2266

; See core-ext
