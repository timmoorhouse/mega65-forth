( Some Stuff )
\ 51 52 53 99 .s cr

include core.f

\ : Foo2 ( -- u) 5 3 if 7 else 9 then 1+ ;
\ : foo2 ( -- ) 5 4 begin .s again ;
\ : foo2 0 begin .s 1+ dup 10 > until drop ;
: foo2 0 begin  1+ dup 10 < while .s repeat drop ;
foo2 cr .s cr

savesystem foo,p,w
.( end of bootstrap.f )