;;; file for debug routines and other dead/maybe dead but possibly useful
;;; code for this project

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

;;; these routines would be cool to show the actors for
;;; the intro music ... the characters appear halfway through
;;; the intro music, but it uses up too much memory
#if 0        
;;; color the actors
;;; Y=0 all back
;;; y!=0 standard colors
ColorActors subroutine
        cpy #0
        beq .skip
        lda #CYAN
        sta g1Start+[clrram-screen]
        lda #GREEN
        sta g3Start+[clrram-screen]
        sta g3Start+[clrram-screen]+1
        lda #PURPLE
        sta g4Start+[clrram-screen]
        sta g4Start+[clrram-screen]+1
        lda #RED
        sta g2StartI+clrram
        rts
.skip        
        ldx #4
        lda #BLACK
.loop
        sta clrram+22*11+9,x
        dex
        bpl .loop
        sta g2StartI+clrram
        rts
        ;; hide the actors
        MAC HideActors
        ldy #0
        jsr ColorActors
        ENDM
ShowActors subroutine
        saveX
        saveY
        ldy #1
        jsr ColorActors
        resY
        resX
        rts
#endif
