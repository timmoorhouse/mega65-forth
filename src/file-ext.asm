
; ****************************************************************************
; FILE EXT

; ****************************************************************************
; FILE-STATUS
; (c-addr u -- x ior)
; ANSI 11.6.2.1524

!if ENABLE_FILE_EXT {
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

!if ENABLE_FILE_EXT {
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

!if ENABLE_FILE_EXT {
        +WORD_IMM "include"
W_INCLUDE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<include>"
        !word W_DOTS
}
        !word W_PARSE_NAME

!if DEBUG {
        !word W_2DUP
        +CLITERAL '['
        !word W_EMIT
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
}

        !word W_RSLO
        !word W_OPEN_FILE
        !word W_DROP ; TODO check status
        !word W_TOR ; need to move to return stack since include-file can do arbitrary things to the data stack
        !word W_RAT
        !word W_INCLUDE_FILE
        !word W_RFROM
        !word W_CLOSE_FILE
        !word W_DROP ; TODO check status

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

!if ENABLE_FILE_EXT {
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

!if ENABLE_FILE_EXT {
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

!if ENABLE_FILE_EXT {
        +WORD "require"
W_REQUIRE
        !word DO_COLON
!if 1 {
        !word W_PDOTQ
        +STRING "<require>"
        !word W_DOTS
}       
        ; TODO
        !word W_PARSE_NAME
!if DEBUG {
        !word W_2DUP
        +CLITERAL '['
        !word W_EMIT
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
}

        !word W_RSLO
        !word W_OPEN_FILE
        !word W_DROP ; TODO check status
        !word W_TOR

!if 0 {
        !word W_RAT
        !word W_INCLUDE_FILE
}
!if 0 {
        !word W_RFROM
        !word W_CLOSE_FILE ; TODO !!!!!!!!!!!!
        !word W_DROP ; TODO check status
}
!if 1 {
        !word W_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRED
; (i*x c-addr u -- i*x)
; Forth 2012 11.6.2.2144.50

!if ENABLE_FILE_EXT {
        +WORD "required"
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
