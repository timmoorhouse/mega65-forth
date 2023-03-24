
\ The following words are implemented internally:
\ 2ROT DU<

: 2value ( x1 x2 "<spaces>name" -- ) create , , 
  does> store? @ if 2! 0 store? ! else 2@ then ;

.( ... end of d-double-ext.f ) cr