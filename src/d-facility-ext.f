
\ The following words are implemented internally:
\

\ *************************************************************************** 
\ MEGA65-specific

#147 constant k-clear
#23  constant k-f12
#25  constant k-f13
#26  constant k-f14
#132 constant k-help
#10  constant k-linefeed
#14  constant k-lower-case
#13  constant k-return
#141 constant k-shift-return
#9   constant k-tab
#142 constant k-upper-case

\ *************************************************************************** 

: +field ( n1 n2 "<spaces>name" -- n3 ) create over , + does> @ + ;

: begin-structure ( "<spaces>name" -- struct-sys 0 ) create here 0 0 , does> @ ;

\ TODO CFIELD:

\ TODO EKEY

\ TODO EKEY>CHAR

\ TODO EKEY>FKEY

\ TODO EKEY?

\ TODO EMIT?

: end-structure ( struct-sys +n -- ) swap ! ;

\ TODO FIELD:

\ TODO K-ALT-MASK

\ TODO K-CTRL-MASK

#20  constant k-delete
#17  constant k-down
\ TODO K-END
#133 constant k-f1
#21  constant k-f10
#22  constant k-f11
#137 constant k-f2
#134 constant k-f3
#138 constant k-f4
#135 constant k-f5
#139 constant k-f6
#136 constant k-f7
#140 constant k-f8
#16  constant k-f9
#19  constant k-home
#148 constant k-insert
#157 constant k-left
\ TODO K-NEXT      \ use f12?
\ TODO K-PRIOR     \ use f10?
#29  constant k-right
\ TODO K-SHIFT-MASK
#145 constant k-up

\ TODO MS

\ TODO TIME&DATE

.( ... end of d-facility-ext.f ) cr