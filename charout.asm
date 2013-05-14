;;; display a number in A on screen
;;; bit 7 on will call to reverse characters
;;; X location to display
DisplayNum16 SUBROUTINE
        sta DSPL_3
        lda #zeroDigit
        sta DSPL_1
        sta DSPL_2
        
        lda DSPL_3
.hundreds
        sta DSPL_3
        sec
        sbc #16
        bcc .ones
        inc DSPL_1
        jmp .hundreds
.ones
        lda DSPL_3
.onesloop
        sta DSPL_3
        sec
        sbc #10
        bcc .done
        inc DSPL_2
        jmp .onesloop
.done
        lda #zeroDigit
        clc
        adc DSPL_3
        sta DSPL_3
        ;; ones is now in S4
        lda DSPL_1
        sta screen,X
        lda #PURPLE
        sta clrram,X
        lda DSPL_2
        inx
        sta screen,x
        lda #PURPLE
        sta clrram,X
        lda DSPL_3
        inx
        sta screen,x
        lda #PURPLE
        sta clrram,X
        rts
