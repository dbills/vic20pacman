;;; common utility macros
                ;; logical not of 1, used to switch between on/off states
        MAC Invert
        lda #1                  ;dbl buffering, switch sprite tiles
        eor {1}                 ; 0 = 1
        sta {1}                 ; or 1 = 0
        ENDM
        ;; save X
        ;; don't call this recursively, use it
        ;; to save 2 or 3  cycles if that matter
        ;; txa,pha = 5 cycles, stx zero page = 3
        ;; pla,tax = 6
        MAC saveX               
        stx Xsave
        ENDM
        ;; restore X
        MAC resX
        ldx Xsave
        ENDM
        MAC saveAll
        pha
        txa
        pha
        tya
        pha
        ENDM
        MAC resAll
        pla
        tay
        pla
        tax
        pla
        ENDM
        ;; make a number negative ( if it isn't ) by creating the 2's complement of it
        ;; number in A
        MAC MakeNegative
        bmi .done
        eor #$ff
        clc
        adc #1
.done
        ENDM
        ;; abs of number in A
        MAC Abs
        bpl .done
        eor #$ff
        clc
        adc #1
.done
        ENDM
        ;; ABORT instruction
        MAC abort
        lda #$c0
        sta VICSCRN
        brk
        ENDM
