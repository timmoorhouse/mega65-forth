
\ The following words are implemented in bootstrap1.f:
\ AHEAD

: [defined] parse-name find-name 0<> ; immediate

: [then] ( -- ) ; immediate

: [else] ( -- )
     1 begin                                          \ level
       begin parse-name dup while                     \ level addr len
         2dup lower
         2dup s" [if]" compare 0= if                  \ level addr len
             2drop 1+                                 \ level'
          else                                        \ level addr len
            2dup s" [else]" compare 0= if             \ level addr len
                2drop 1- dup if 1+ then               \ level'
            else                                      \ level addr len
                s" [then]" compare 0= if              \ level
                   1-                                 \ level'
               then
             then
          then ?dup 0= if exit then                   \ level'
       repeat 2drop                                   \ level
    refill 0= until                                   \ level
    drop
; immediate
 
: [if] ( flag | flag "<spaces>name ..." ) 0= if postpone [else] then ; immediate

: [undefined] parse-name find-name 0= ; immediate

.( ... end of d-tools-ext.f ) cr