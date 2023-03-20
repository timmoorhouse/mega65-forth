
\ TODO file-status

: flush-file ( fileid -- ior ) drop 0 ; \ TODO

: include ( "<spaces>name" -- ) parse-name included ;

\ TODO rename-file

\ : require ( "<spaces>name" -- ) parse-name required ;

\ TODO required

.( ... end of d-file-ext.f ) cr