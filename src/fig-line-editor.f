
     SCR # 87
       0 (  TEXT,  LINE                                    WFR-79MAY01 )
       1 FORTH  DEFINITIONS   HEX
       2 : TEXT                        ( ACCEPT FOLLOWING TEXT TO PAD *)
       3      HERE  C/L  1+   BLANKS  WORD  HERE  PAD  C/L  1+  CMOVE  ;
       4
       5 : LINE              ( RELATIVE TO SCR, LEAVE ADDRESS OF LINE *)
       6       DUP  FFF0  AND  17  ?ERROR   ( KEEP ON THIS SCREEN )
       7       SCR  @  (LINE)  DROP  ;
       8 -->
       9
      10
      11
      12
      13
      14
      15

     SCR # 88
       0 (  LINE EDITOR                                    WFR-79MAY03 )
       1 VOCABULARY  EDITOR  IMMEDIATE    HEX
       2 : WHERE                  ( PRINT SCREEN # AND IMAGE OF ERROR *)
       3     DUP  B/SCR  /  DUP  SCR  !  ." SCR # "  DECIMAL  .
       4     SWAP  C/L  /MOD  C/L  *  ROT  BLOCK  +  CR  C/L  TYPE
       5     CR  HERE  C@  -  SPACES  5E EMIT  [COMPILE] EDITOR  QUIT  ;
       6
       7 EDITOR  DEFINITIONS
       8 : #LOCATE                    ( LEAVE CURSOR OFFSET-2, LINE-1 *)
       9          R#  @  C/L  /MOD  ;
      10 : #LEAD                 ( LINE ADDRESS-2, OFFSET-1 TO CURSOR *)
      11          #LOCATE  LINE  SWAP  ;
      12 : #LAG              ( CURSOR ADDRESS-2, COUNT-1 AFTER CURSOR *)
      13          #LEAD  DUP  >R  +  C/L  R>  -  ;
      14 : -MOVE      ( MOVE IN BLOCK BUFFER ADDR FROM-2,  LINE TO-1 *)
      15          LINE  C/L  CMOVE  UPDATE  ;  -->

     SCR # 89
       0 (  LINE EDITING COMMANDS                          WFR-79MAY03 )
       1 : H                              ( HOLD NUMBERED LINE AT PAD *)
       2       LINE  PAD  1+  C/L  DUP  PAD  C!  CMOVE  ;
       3
       4 : E                               ( ERASE LINE-1 WITH BLANKS *)
       5       LINE  C/L  BLANKS  UPDATE  ;
       6
       7 : S                             ( SPREAD MAKING LINE # BLANK *)
       8       DUP  1  -  ( LIMIT )  0E ( FIRST TO MOVE )
       9       DO  I  LINE  I  1+  -MOVE  -1  +LOOP  E  ;
      10
      11 : D                         ( DELETE LINE-1, BUT HOLD IN PAD *)
      12       DUP  H  0F  DUP  ROT
      13       DO  I  1+  LINE  I  -MOVE  LOOP  E  ;
      14
      15 -->

     SCR # 90
       0 (  LINE EDITING COMMANDS                          WFR-79MAY03 )
       1
       2 : M    ( MOVE CURSOR BY SIGNED AMOUNT-1, PRINT ITS LINE *)
       3      R#  +!  CR  SPACE  #LEAD  TYPE  5F  EMIT
       4                         #LAG   TYPE  #LOCATE  .  DROP  ;
       5
       6 : T    ( TYPE LINE BY #-1,  SAVE ALSO IN PAD *)
       7      DUP  C/L  *  R#  !  DUP  H  0  M  ;
       8
       9 : L     ( RE-LIST SCREEN *)
      10         SCR  @  LIST  0  M  ;
      11 -->
      12
      13
      14
      15

     SCR # 91
       0 (  LINE EDITING COMMANDS                           WFR-790105 )
       1 : R                          ( REPLACE ON LINE #-1, FROM PAD *)
       2       PAD  1+  SWAP  -MOVE  ;
       3
       4 : P                           ( PUT FOLLOWING TEXT ON LINE-1 *)
       5       1  TEXT  R  ;
       6
       7 : I                       ( INSERT TEXT FROM PAD ONTO LINE # *)
       8       DUP  S  R  ;
       9                             CR
      10 : TOP                    ( HOME CURSOR TO TOP LEFT OF SCREEN *)
      11       0  R#  !  ;
      12 -->
      13
      14
      15

     SCR # 92
       0 (  SCREEN EDITING COMMANDS                        WFR-79APR27 )
       1 : CLEAR                           ( CLEAR SCREEN BY NUMBER-1 *)
       2       SCR  !  10  0  DO  FORTH  I  EDITOR  E  LOOP  ;
       3
       4 : FLUSH                   ( WRITE ALL UPDATED BLOCKS TO DISC *)
       5     [  LIMIT  FIRST  -  B/BUF  4  +  /  ]  ( NUMBER OF BUFFERS)
       6     LITERAL  0  DO  7FFF  BUFFER  DROP  LOOP  ;
       7
       8 : COPY                   ( DUPLICATE SCREEN-2, ONTO SCREEN-1 *)
       9    B/SCR  *  OFFSET  @  +  SWAP  B/SCR  *  B/SCR  OVER  +  SWAP
      10    DO  DUP  FORTH  I  BLOCK  2  -  !  1+   UPDATE  LOOP
      11    DROP  FLUSH  ;
      12 -->
      13
      14
      15

     SCR # 93
       0 (  DOUBLE NUMBER SUPPORT                          WFR-80APR24 )
       1 (  OPERATES ON 32 BIT DOUBLE NUMBERS   OR TWO 16-BIT INTEGERS )
       2 FORTH DEFINITIONS
       3
       4 : 2DROP   DROP    DROP  ;  ( DROP DOUBLE NUMBER )
       5
       6 : 2DUP    OVER    OVER  ;  ( DUPLICATE A DOUBLE NUMBER )
       7
       8 : 2SWAP   ROT     >R    ROT   R>  ;
       9         ( BRING SECOND DOUBLE TO TOP OF STACK )
      10 EDITOR DEFINITIONS  -->
      11
      12
      13
      14
      15

     SCR # 94
       0 (  STRING MATCH FOR EDITOR                     PM-WFR-80APR25 )
       1 : -TEXT                   ( ADDRESS-3, COUNT-2, ADDRESS-1 --- )
       2  SWAP   -DUP  IF  ( LEAVE BOOLEAN MATCHED-NON-ZERO, NOPE-ZERO )
       3               OVER + SWAP       (NEITHER ADDRESS MAY BE ZERO! )
       4         DO  DUP  C@  FORTH  I  C@  -
       5             IF  0=  LEAVE  ELSE  1+  THEN    LOOP
       6         ELSE  DROP  0=  THEN
       7 : MATCH   ( CURSOR ADDRESS-4, BYTES LEFT-3, STRING ADDRESS-2, )
       8           ( STRING COUNT-1, ---  BOOLEAN-2, CURSOR MOVEMENT-1 )
       9   >R  >R  2DUP  R>  R>  2SWAP  OVER  +  SWAP
      10   ( CADDR-6, BLEFT-5, $ADDR-4, $LEN-3, CADDR+BLEFT-2, CADDR-1 )
      11   DO  2DUP  FORTH   I   -TEXT
      12     IF  >R  2DROP  R>  -  I  SWAP  -  0  SWAP  0  0  LEAVE
      13         (  CADDR BLEFT  $ADDR  $LEN  OR ELSE 0  OFFSET  0  0  )
      14       THEN  LOOP 2DROP   ( CADDR-2, BLEFT-1, OR 0-2, OFFSET-1 )
      15     SWAP  0=  SWAP  ;    -->

     SCR # 95
       0 (  STRING EDITING COMMANDS                        WFR-79MAR24 )
       1 : 1LINE       ( SCAN LINE WITH CURSOR FOR MATCH TO PAD TEXT, *)
       2                              ( UPDATE CURSOR, RETURN BOOLEAN *)
       3        #LAG  PAD  COUNT  MATCH  R#   +!   ;
       4
       5 :  FIND   ( STRING AT PAD OVER FULL SCREEN RANGE, ELSE ERROR *)
       6      BEGIN  3FF  R#  @  <
       7          IF  TOP  PAD  HERE  C/L  1+  CMOVE  0  ERROR  ENDIF
       8          1LINE   UNTIL   ;
       9
      10 : DELETE                    ( BACKWARDS AT CURSOR BY COUNT-1 *)
      11     >R  #LAG  +  FORTH  R  -  ( SAVE BLANK FILL LOCATION )
      12     #LAG  R MINUS  R#  +!     ( BACKUP CURSOR )
      13     #LEAD  +  SWAP  CMOVE
      14     R>  BLANKS  UPDATE  ;   ( FILL FROM END OF TEXT )
      15 -->

     SCR # 96
       0 (  STRING EDITOR COMMANDS                         WFR-79MAR24 )
       1 : N     ( FIND NEXT OCCURANCE OF PREVIOUS TEXT *)
       2       FIND  0  M  ;
       3
       4 : F      ( FIND OCCURANCE OF FOLLOWING TEXT *)
       5       1  TEXT  N  ;
       6
       7 : B      ( BACKUP CURSOR BY TEXT IN PAD *)
       8       PAD  C@  MINUS  M  ;
       9
      10 : X     ( DELETE FOLLOWING TEXT *)
      11       1  TEXT  FIND  PAD  C@  DELETE  0  M  ;
      12
      13 : TILL      ( DELETE ON CURSOR LINE, FROM CURSOR TO TEXT END *)
      14       #LEAD  +  1  TEXT  1LINE  0=  0  ?ERROR
      15       #LEAD  +  SWAP  -  DELETE  0  M  ;     -->

     SCR # 97
       0 (  STRING EDITOR COMMANDS                         WFR-79MAR23 )
       1 : C        ( SPREAD AT CURSOR AND COPY IN THE FOLLOWING TEXT *)
       2     1  TEXT  PAD  COUNT
       3     #LAG  ROT  OVER  MIN  >R
       4     FORTH  R  R#  +!  ( BUMP CURSOR )
       5     R  -  >R          ( CHARS TO SAVE )
       6     DUP  HERE  R  CMOVE  ( FROM OLD CURSOR TO HERE )
       7     HERE  #LEAD  +  R>  CMOVE  ( HERE TO CURSOR LOCATION )
       8     R>  CMOVE  UPDATE   ( PAD TO OLD CURSOR )
       9     0  M  ( LOOK AT NEW LINE )  ;
      10 FORTH  DEFINITIONS   DECIMAL
      11 LATEST   12  +ORIGIN  !   ( TOP NFA )
      12 HERE     28  +ORIGIN  !   ( FENCE )
      13 HERE     30  +ORIGIN  !   ( DP )
      14 '  EDITOR  6  +   32  +ORIGIN  !  ( VOC-LINK )
      15 HERE  FENCE   !      ;S

     SCR # 98
       0
       1
       2
       3
       4
       5
       6
       7
       8
       9
      10
      11
      12
      13
      14
      15

