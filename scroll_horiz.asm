
scroll_horiz SUBROUTINE
        lda Sprite_dir,X
        cmp #1
        beq .ok                 ;ok to move horizontal
        ;; switch to horiz order
        lda SPRT_LOCATOR
        sta S1
        jsr changehoriz
        bcc .ok
        rts
.ok
        ldy Sprite_offset,X
        cpy END_SCRL_VAL
        bne .draw
.course                         ;course scroll
        move16x Sprite_loc,W2   ;
        lda SCRL_VAL
        bmi .left
        ;; going right
        lda #0
        pha
        inc16 W2
        ldy #01       
        lda #MW
        cmp (W2),Y              ;check for wall at pos + 2
        bne .continue
.cantmove        
        sec                     ;can't move, return false
        pla                     ;pop the single arg we pushed for our own use
        rts
.left
        lda #8
        pha
        dec16 W2                ;check to the left for wall
        lda #MW
        ldy #0
        cmp (W2),Y
        beq .cantmove
.continue
        
        move16x2 W2,Sprite_loc2  ;save the new sprite screen location
        pla                      ;pull new sprite offset from the stack
        tay
.draw
        tya
        clc
        adc SCRL_VAL
        sta Sprite_offset2,X
        clc
        rts

