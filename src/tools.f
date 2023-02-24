
\ TODO traverse-namelist should do the hidden check
\ : print-name ( nt -- u ) dup ?hidden =0 if
\         out @ 
\             dup if space then
\             c/l greater if cr then
\         name>string type
\     then 
\     true
\ ;
\ 
\ : words ( -- ) ' print-name forth-wordlist traverse-wordlist ;

.( end of tools.f )