;;; move sprite side to side
        MAC moveS
        lda Sprite_motion,X
        cmp #motionRight
        beq .right
        jsr scroll_left
        bcc .done
        lda #motionRight
        sta Sprite_motion,X
.right
        jsr scroll_right
        bcc .done
        lda #motionLeft
        sta Sprite_motion,X
.done        
        ENDM
        
;;; move sprite up and down
        MAC moveD
        lda Sprite_motion,X
        cmp #22
        beq .down
        jsr scroll_up
        bcs .reverse
        bcc .done
.down
        jsr scroll_down
        bcs .reverse
        bcc .done
.reverse
        lda Sprite_motion,X
        cmp #22
        beq .0
        bne .1
.0
        lda #$EA                 ;-22
        jmp .store
.1
        lda #22
.store        
        sta Sprite_motion,X
.done
        ENDM