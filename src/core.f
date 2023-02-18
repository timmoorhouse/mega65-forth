

\ : begin here 1 ; immediate


\ ***************************************************************************
\ ... if [ ... else ] ... then

\ : if postpone 0branch here 0 , 2 ; immediate

\ : else postpone branch here 0 , swap 2 postpone endif 2 ; immediate

\ : then here over - swap ! ; immediate

\ ***************************************************************************

\ : spaces ( n -- ) 0 max ?dup if 0 do space loop then ;
    
.( end of core.f )