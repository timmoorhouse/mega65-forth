; ****************************************************************************
; FACILITY EXT

K_CLEAR      = 147
K_DELETE     =  20
K_DOWN       =  17    
K_F1         = 133
K_F10        =  21  
K_F11        =  22  
K_F12        =  23 
K_F13        =  25
K_F14        =  26 
K_F2         = 137  
K_F3         = 134    
K_F4         = 138
K_F5         = 135  
K_F6         = 139  
K_F7         = 136  
K_F8         = 140  
K_F9         =  16  
K_HOME       =  19    
K_INSERT     = 148  
K_LEFT       = 157 
K_LINEFEED   =  10    
K_RIGHT      =  29
K_RETURN     =  13
K_TAB        =   9
K_UP         = 145   

; TODO - lookup table of KEY/EKEY to handler?  could then swap it out in a visual editor

; TODO !!!!!
!if ENABLE_FACILITY {
        +WORD "k-clear"
W_K_CLEAR
        !word DO_CONSTANT
        !word K_CLEAR

        +WORD "k-f13"
W_K_F13
        !word DO_CONSTANT
        !word K_F13

        +WORD "k-f14"
W_K_F14
        !word DO_CONSTANT
        !word K_F14

        +WORD "k-linefeed"
W_K_LINEFEED
        !word DO_CONSTANT
        !word K_LINEFEED

        +WORD "k-tab"
W_K_TAB
        !word DO_CONSTANT
        !word K_TAB
}

; TODO !!!!!
!if ENABLE_FACILITY {
        +WORD "k-return"
} else {
        +NONAME
}
W_K_RETURN
        !word DO_CONSTANT
        !word K_RETURN

; ****************************************************************************
; +FIELD
; Forth 2012 10.6.2.0135

; See reference implementation

!if ENABLE_FACILITY {
}

; ****************************************************************************
; BEGIN-STRUCTURE
; Forth 2012 10.6.2.0763

; See reference implementation

!if ENABLE_FACILITY {
}

; ****************************************************************************
; CFIELD:
; Forth 2012 10.6.2.0893

!if ENABLE_FACILITY {
}

; ****************************************************************************
; EKEY
; (-- u)
; ANSI 10.6.2.1305

; TODO MSB=modifier bits, LSB=ASCII? keyboard matrix?
!if ENABLE_FACILITY {
}

; ****************************************************************************
; EKEY>CHAR
; (u -- u false | char true)
; ANSI 10.6.2.1306

!if ENABLE_FACILITY {
}

; ****************************************************************************
; EKEY>FKEY
; Forth 2012 10.6.2.1306.40

!if ENABLE_FACILITY {
}

; ****************************************************************************
; EKEY?
; (-- flag)
; ANSI 10.6.2.1307

!if ENABLE_FACILITY {
}

; ****************************************************************************
; EMIT?
; (-- flag)
; ANSI 10.6.2.1325

!if ENABLE_FACILITY {
}

; ****************************************************************************
; END-STRUCTURE
; Forth 2012 10.6.2.1336

; See reference implementation

!if ENABLE_FACILITY {
}

; ****************************************************************************
; FIELD:
; Forth 2012 10.6.2.1518

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-ALT-MASK
; Forth 2012 10.6.2.1740.01

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-CTRL-MASK
; Forth 2012 10.6.2.1740.02

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-DELETE
; Forth 2012 10.6.2.1740.03

!if ENABLE_FACILITY {
        +WORD "k-delete"
}
W_K_DELETE
        !word DO_CONSTANT
        !word K_DELETE

; ****************************************************************************
; K-DOWN
; Forth 2012 10.6.2.1740.04

!if ENABLE_FACILITY {
        +WORD "k-down"
W_K_DOWN
        !word DO_CONSTANT
        !word K_DOWN
}

; ****************************************************************************
; K-END
; Forth 2012 10.6.2.1740.05

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-F1
; Forth 2012 10.6.2.1740.06

!if ENABLE_FACILITY {
        +WORD "k-f1"
W_K_F1
        !word DO_CONSTANT
        !word K_F1    
}

; ****************************************************************************
; K-F10
; Forth 2012 10.6.2.1740.07

!if ENABLE_FACILITY {
        +WORD "k-f10"
W_K_F10
        !word DO_CONSTANT
        !word K_F10 
}

; ****************************************************************************
; K-F11
; Forth 2012 10.6.2.1740.08

!if ENABLE_FACILITY {
        +WORD "k-f11"
W_K_F11
        !word DO_CONSTANT
        !word K_F11    
}

; ****************************************************************************
; K-F12
; Forth 2012 10.6.2.1740.09

!if ENABLE_FACILITY {
        +WORD "k-f12"
W_K_F12
        !word DO_CONSTANT
        !word K_F12         
}

; ****************************************************************************
; K-F2
; Forth 2012 10.6.2.1740.10

!if ENABLE_FACILITY {
        +WORD "k-f2"
W_K_F2
        !word DO_CONSTANT
        !word K_F2      
}

; ****************************************************************************
; K-F3
; Forth 2012 10.6.2.1740.11

!if ENABLE_FACILITY {
        +WORD "k-3"
W_K_F3
        !word DO_CONSTANT
        !word K_F3      
}

; ****************************************************************************
; K-F4
; Forth 2012 10.6.2.1740.12

!if ENABLE_FACILITY {
        +WORD "k-f4"
W_K_F4
        !word DO_CONSTANT
        !word K_F4      
}

; ****************************************************************************
; K-F5
; Forth 2012 10.6.2.1740.13

!if ENABLE_FACILITY {
        +WORD "k-f5"
W_K_F5
        !word DO_CONSTANT
        !word K_F5   
}

; ****************************************************************************
; K-F6
; Forth 2012 10.6.2.1740.14

!if ENABLE_FACILITY {
        +WORD "k-f6"
W_K_F6
        !word DO_CONSTANT
        !word K_F6           
}

; ****************************************************************************
; K-F7
; Forth 2012 10.6.2.1740.15

!if ENABLE_FACILITY {
        +WORD "k-f7"
W_K_F7
        !word DO_CONSTANT
        !word K_F7       
}

; ****************************************************************************
; K-F8
; Forth 2012 10.6.2.1740.16

!if ENABLE_FACILITY {
        +WORD "k-f8"
W_K_F8
        !word DO_CONSTANT
        !word K_F8       
}

; ****************************************************************************
; K-F9
; Forth 2012 10.6.2.1740.17

!if ENABLE_FACILITY {
        +WORD "k-f9"
W_K_F9
        !word DO_CONSTANT
        !word K_F9       
}

; ****************************************************************************
; K-HOME
; Forth 2012 10.6.2.1740.18

!if ENABLE_FACILITY {
        +WORD "k-home"
W_K_HOME
        !word DO_CONSTANT
        !word K_HOME     
}

; ****************************************************************************
; K-INSERT
; Forth 2012 10.6.2.1740.19

!if ENABLE_FACILITY {
        +WORD "k-insert"
W_K_INSERT
        !word DO_CONSTANT
        !word K_INSERT   
}

; ****************************************************************************
; K-LEFT
; Forth 2012 10.6.2.1740.20

!if ENABLE_FACILITY {
        +WORD "k-left"
W_K_LEFT
        !word DO_CONSTANT
        !word K_LEFT    
}
  
; ****************************************************************************
; K-NEXT
; Forth 2012 10.6.2.1740.21

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-PRIOR
; Forth 2012 10.6.2.1740.22

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-RIGHT
; Forth 2012 10.6.2.1740.23

!if ENABLE_FACILITY {
        +WORD "k-right"
W_K_RIGHT
        !word DO_CONSTANT
        !word K_RIGHT       
}

; ****************************************************************************
; K-SHIFT-MASK
; Forth 2012 10.6.2.1740.24

!if ENABLE_FACILITY {
}

; ****************************************************************************
; K-UP
; Forth 2012 10.6.2.1740.25

!if ENABLE_FACILITY {
        +WORD "k-up"
W_K_UP
        !word DO_CONSTANT
        !word K_UP      
}

; ****************************************************************************
; MS
; (u --)
; ANSI 10.6.2.1905

!if ENABLE_FACILITY {
}

; ****************************************************************************
; TIME&DATE
; (-- +n_1 +n_2 +n_3 +n_4 +n_5 +n_6)
; ANSI 10.6.2.2292

!if ENABLE_FACILITY {
}
