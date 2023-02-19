( Some Stuff )
\ 51 52 53 99 .s cr

include core.f

: Foo2 ( -- u) 5 3 if ( 7 else ) 9 then 1+ ;
foo2 cr .s cr

savesystem foo,p,w
.( end of bootstrap.f )