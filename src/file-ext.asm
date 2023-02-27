
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
        +DOTQ "<file-status>"
        !word W_SIMPLE_DOTS
}       
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
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
        +DOTQ "<flush-file>"
        !word W_SIMPLE_DOTS
}       
        ; TODO
        !word W_DROP
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
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
        !word W_PARSE_NAME
        !word W_INCLUDED
        !word W_PSEMI     
}

; ****************************************************************************
; REFILL
; (-- flag)

; See core-ext

; ****************************************************************************
; RENAME-FILE
; (c-addr_1 u_1 c-addr_2 u_2 -- ior)
; ANSI 11.6.2.2130

!if ENABLE_FILE_EXT {
        +WORD "rename-file"
W_RENAME_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<rename-file>"
        !word W_SIMPLE_DOTS
}       
        ; TODO
        !word W_2DROP        
        !word W_2DROP
        !word W_ZERO    
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
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
!if DEBUG {
        +DOTQ "<require>"
        !word W_SIMPLE_DOTS
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
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; REQUIRED
; (i*x c-addr u -- i*x)
; Forth 2012 11.6.2.2144.50

; See reference implementation

!if ENABLE_FILE_EXT {
        +WORD "required"
W_REQUIRED
        !word DO_COLON
!if DEBUG {
        +DOTQ "<required>"
        !word W_SIMPLE_DOTS
}       
        ; TODO
        !word W_2DROP 
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI    
}

; ****************************************************************************
; S\"
; Forth 2012 11.6.2.2266

; See core-ext
