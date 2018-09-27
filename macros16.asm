;;; macros for manipulaing 16 bit quantites

;;; compare {1} with #{2}
;;; you can use beq,bne with this
        MAC cmp16Im
        lda {1}+1
        cmp #{2} >> 8       ; load high byte
        bne .done
        lda {1}
        cmp #[{2}] & $ff    ; load low byte
.done        
        ENDM
;;; compare word in {1} with {2}
        MAC cmp16
        lda {1}+1
        cmp {2}+1
        bne .done
        lda {1}
        cmp {2}
.done        
        ENDM
; write a 16 bit address to a destination
; indexed by X
; store16x(source_label,dest[X])
    mac store16x
    txa                         ; x * 2 since 16 bits
    pha
    asl
    tax
    lda #[{1}] & $ff            ; load low byte
    sta {2},X                   ; store low byte
    lda #[{1}] >> 8             ; load high byte
    inx
    sta [{2}],X                 ; store high byte
    pla
    tax                         ; restore X register
    endm
;;;  same as store16x
    mac store16y
    tya                         ; x * 2 since 16 bits
    pha
    asl
    tay
    lda #[{1}] & $ff            ; load low byte
    sta {2},y                   ; store low byte
    lda #[{1}] >> 8             ; load high byte
    iny
    sta [{2}],y                 ; store high byte
    pla
    tay                         ; restore Y register
    endm
;;; write 16 bit address to dest
;;; (source,dest)
    mac store16
    lda #[{1}] & $ff    ; load low byte
    sta {2}             ; store low byte
    lda #[{1}] >> 8     ; load high byte
    sta [{2}]+1         ; store high byte
    endm
    mac inc16
    INC [{1}]+0
    BNE .done
    INC [{1}]+1
.done
    endm
    ;; move 16
    mac move16
    lda [{1}]
    sta [{2}]
    lda [{1}]+1
    sta [{2}]+1
    endm
    ;; move word index by X, does correct pointer
    ;; arithmetic for 'word'
    ;; move16x source[X],dest
    ;;
    ;; C equiv:
    ;; unsigned char x;
    ;; unsigned short *souce[],*dest;
    ;; dest = source[x];
    ;; uses Y.  =X on return
    mac move16x
    txa
    tay                         ; PGOTCHA: replace with pha if problems
    asl                         ; x*2 since it's a word
    tax
    lda [{1}],X
    sta [{2}]
    inx
    lda [{1}],X
    sta [{2}]+1
    tya                     ; PGOTCHA: replace with pla if problems
    tax
    endm
    ;; move word index by Y, does correct pointer
    ;; arithmetic for 'word'
    ;; move16y source[Y],dest
    mac move16y
    tya
    pha
    asl                         ; x*2 since it's a word
    tay
    lda [{1}],y
    sta [{2}]
    iny
    lda [{1}],y
    sta [{2}]+1
    pla
    tay
    endm
;;; move source[x],dest[x]
    mac move16xx
    txa
    pha
    asl                         ; x*2 since it's a word
    tax
    lda [{1}],x
    sta [{2}],x
    inx
    lda [{1}],x
    sta [{2}],x
    pla
    tax
    endm
;;; move source,dest[x]
    mac move16x2
    txa
    pha
    asl                         ; x*2 since it's a word
    tax
    lda [{1}]
    sta [{2}],x
    inx
    lda [{1}]+1
    sta [{2}],x
    pla
    tax
    endm
    ;; 16 bit add {1}+{2} result in {1}
    mac add
        CLC                     ;Ensure carry is clear
        LDA [{1}]+0             ;Add the two least significant bytes
        ADC [{2}]+0             ;
        STA [{1}]+0             ;... and store the result
        LDA [{1}]+1             ;Add the two most significant bytes
        ADC [{2}]+1             ;... and any propagated carry bit
        STA [{1}]+1             ;... and store the result    clc
    endm
;; usage add source,dest,X
;;; source +x into dest
    mac addxx
        CLC                ;Ensure carry is clear
        LDA [{1}]+0        ;Add the two least significant bytes
        ADC {3}            ;
        STA [{2}]+0        ;... and store the result
        LDA [{1}]+1        ;Add the two most significant bytes
        ADC #0             ;... and any propagated carry bit
        STA [{2}]+1        ;... and store the result    clc
    endm
        
;;; subtract 8 bit {3} from 16 bit number {1}, output in {2}
    mac subxx
        SEC
        LDA [{1}]+0
        SBC [{3}]+0
        STA [{2}]+0
        LDA [{1}]+1
        SBC #0
        STA [{2}]+1
    endm
        ;; substract byte {2} from word
        ;; {1} input, also contains result
        ;; 
        MAC sub16
        sec
        lda {1}
        sbc {2}
        sta {1}
        lda {1}+1
        sbc {2}+1
        sta {1}+1
        ENDM

        MAC add16Im
        clc
        lda {1}
        adc #[{2}]&$ff
        sta {1}
        lda {1}+1
        adc #[{2}]>>8
        sta {1}+1
        ENDM

        MAC sub16Im
        sec
        lda {1}
        sbc #[{2}]&$ff
        sta {1}
        lda {1}+1
        sbc #[{2}]>>8
        sta {1}+1
        ENDM


        MAC dec16z
        lda  [{1}]+0            ;load lsb
        bne .notzero
        ;; double decrement needed
        dec [{1}]+1             ;dec msb
.notzero
        dec [{1}]+0             ;dec lsb
        beq .lsbzero
        bne .done
.lsbzero
        ;; lsb was 0, load msb, it it's also zero
        ;; the Z flag will correct for 16 bit number
        lda {1}+1         
.done        
        ENDM
        ;; duh, you can't use beq with this
        MAC dec16
        lda  [{1}]+0
        bne .done
        dec [{1}]+1
.done
        dec [{1}]+0
        ENDM

