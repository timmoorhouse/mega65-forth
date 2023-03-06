
;
; Things only needed during bootstrapping
;
; Everything in this file must be unnecessary once boostrapping ends
; or bad things will happen
;

; TODO move rdump from debug here, add a nicer forth implementation

; TODO can we move put_string here?

; TODO move to volatile area
W_SIMPLE_DOT
        !word *+2
        lda #'$'
        jsr EMIT
        lda 1,x
        jsr put_hex
        lda 0,x
        jsr put_hex
        jmp POP

        +NONAME
W_SIMPLE_DOTS
        !word *+2
        jsr SIMPLE_DOTS
        jmp NEXT

SIMPLE_DOTS
        stx <XSAVE
        lda #' '
        jsr EMIT
        lda #'<'
        jsr EMIT
        lda #TOS
        sec
        sbc XSAVE
        lsr
        jsr put_hex
        lda #'>'
        jsr EMIT

        lda #TOS
        sta <TEMP1

-       lda <TEMP1
        cmp <XSAVE
        beq +

        lda #' '
        jsr EMIT
        dec <TEMP1
        dec <TEMP1
        ldy <TEMP1
        lda base_page+1,y
        jsr put_hex
        ldy <TEMP1
        lda base_page,y
        jsr put_hex
        jmp -
+        
        rts


        +NONAME
W_AUTOBOOT_BOOTSTRAP
        !word DO_COLON

        !word W_DECIMAL

        +CSLITERAL "autoboot.f"
        !word W_COUNT
        +LITERAL W_INCLUDED
        !word W_CATCH
        !word W_QDUP
        +ZBRANCH +
        !word W_REPORT_EXCEPTION
+
        !word W_PSEMI
