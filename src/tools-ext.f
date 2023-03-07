

\ HEAD see core.f

: [defined] parse-name find-name 0<> ; immediate

\ : [then] ( -- ) ; immediate

\ : [else] ( -- )
\      1 begin                                          \ level
\        begin bl word count dup while                  \ level adr len
\          2dup s" [if]" compare 0= if                  \ level adr len
\              2drop 1+                                 \ level'
\           else                                        \ level adr len
\             2dup s" [else]" compare 0= if             \ level adr len
\                 2drop 1- dup if 1+ then               \ level'
\             else                                      \ level adr len
\                 s" [then]" compare 0= if              \ level
\                    1-                                 \ level'
\                then
\              then
\           then ?dup 0= if exit then                   \ level'
\        repeat 2drop                                   \ level
\     refill 0= until                                   \ level
\     drop
\ ; immediate
 
\ : [if] ( flag | flag "<spaces>name ..." ) 0= if postpone [else] then ; immediate

: [undefined] parse-name find-name 0= ; immediate

.( ... end of tools-ext.f ) cr