

; intermission music

; TODO use 1 freq table, as there are significant shared notes bas v. sop
; 197 202 205 _208 _211 216 218 _220 _222 _224 226 233

; note frequency tables
sopFreqs
        dc.b    0 ; (rest)
        dc.b    197; 199; 195; c
        dc.b    202; 203; 201; d
        dc.b    208; 209; 207; e
        dc.b    211; 212; 209; f
        dc.b    216; 217; 215; g
        dc.b    218; 219; 217; aF
        dc.b    220; 221; 219; a
        dc.b    222; 223; 221; bF
        dc.b    224; 225; 223; b
basFreqs
        dc.b    0 ; (rest)
        dc.b    205; 207; 203; e   e-flat
        dc.b    208; 209; 207; E   e-nat
        dc.b    211; 212; 209; f
        dc.b    220; 221; 219; a
        dc.b    222; 223; 221; b   b-flat
        dc.b    224; 225; 223; B   b-nat
        dc.b    226; 227; 225; c
        dc.b    233; 233; 232; F   f oct high

; song sequence tables
; byte&f0 --> note table lookup
; byte&0f --> duration; 0 marks End Of Sequence

basSequences
basSeqA
        dc.b [3<<4] + 4     ; f4
        dc.b [0<<4] + 8     ; .8
        dc.b [3<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b [3<<4] + 4     ; f4
        dc.b [0<<4] + 8     ; .8
        dc.b [3<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b 0                  ; ..

basSeqB
        dc.b [3<<4] + 4     ; f4
        dc.b [0<<4] + 8     ; .8
        dc.b [3<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 3     ; a3
        dc.b [0<<4] + 1     ; .1
        dc.b [5<<4] + 3     ; b3
        dc.b [0<<4] + 1     ; .1
        dc.b [6<<4] + 3     ; B3
        dc.b [0<<4] + 1     ; .1
        dc.b [7<<4] + 3     ; c3
        dc.b [0<<4] + 1     ; .1
        dc.b 0                  ; ..

basSeqCD
        dc.b [8<<4] + 7     ; F7
        dc.b [0<<4] + 1     ; .1
        dc.b [7<<4] + 3     ; c3
        dc.b [0<<4] + 1     ; .1
        dc.b [5<<4] + 3     ; b3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 3     ; a3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 3     ; a3
        dc.b [0<<4] + 1     ; .1
        dc.b [3<<4] + 7     ; f7
        dc.b [0<<4] + 1     ; .1
        dc.b [1<<4] + 7     ; e7
        dc.b [0<<4] + 1     ; .1
        dc.b [2<<4] + 7     ; E7
        dc.b [0<<4] + 1     ; .1
        dc.b [3<<4] + 8     ; f8
        dc.b [0<<4] + 8     ; .8
        dc.b 0                  ; ..

sopSequences
sopSeqA
        dc.b [3<<4] + 1     ; e1
        dc.b [4<<4] + 7     ; f7
        dc.b [3<<4] + 1     ; e1
        dc.b [4<<4] + 7     ; f7
        dc.b [3<<4] + 1     ; e1
        dc.b [4<<4] + 7     ; f7
        dc.b [1<<4] + 1     ; c1
        dc.b [2<<4] + 3     ; d3
        dc.b [1<<4] + 4     ; c4
        dc.b 0                  ; ..

sopSeqB
        dc.b [4<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 8     ; f8
        dc.b [6<<4] + 1     ; G1
        dc.b [7<<4] + 11     ; a11
        dc.b [0<<4] + 8     ; .4
        dc.b 0                  ; ..

sopSeqC
        dc.b [4<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 8     ; f8
        dc.b [1<<4] + 1     ; c1
        dc.b [2<<4] + 11     ; d11
        dc.b [0<<4] + 8     ; .4
        dc.b 0                  ; ..

sopSeqDEF
        dc.b [4<<4] + 3     ; f3
        dc.b [0<<4] + 1     ; .1
        dc.b [4<<4] + 8     ; f8
        dc.b [5<<4] + 1     ; g1
        dc.b [6<<4] + 7     ; G7
        dc.b [8<<4] + 7     ; b7
        dc.b [0<<4] + 1     ; .1
        dc.b [9<<4] + 11     ; B11
        dc.b [0<<4] + 1     ; .1
        dc.b [6<<4] + 1     ; G1
        dc.b [8<<4] + 6     ; b6
        dc.b [0<<4] + 1     ; .1
        dc.b [5<<4] + 1     ; g1
        dc.b [6<<4] + 7     ; G7
        dc.b [4<<4] + 8     ; f8
        dc.b [5<<4] + 1     ; g1
        dc.b [6<<4] + 14     ; G14
        dc.b [0<<4] + 1     ; .1
        dc.b [3<<4] + 1     ; e1
        dc.b [4<<4] + 7     ; f7
        dc.b [0<<4] + 8     ; .8
        dc.b 0                  ; ..

basSeqMap
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b [basSeqCD-basSeqA]
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b 0
        dc.b [basSeqB-basSeqA]
        dc.b [basSeqCD-basSeqA]
        dc.b $FF

sopSeqMap
        dc.b 0
        dc.b [sopSeqB-sopSeqA]
        dc.b 0
        dc.b [sopSeqC-sopSeqA]
        dc.b 0
        dc.b [sopSeqDEF-sopSeqA]
        dc.b 0
        dc.b [sopSeqB-sopSeqA]
        dc.b 0
        dc.b [sopSeqC-sopSeqA]
        dc.b 0
        dc.b [sopSeqDEF-sopSeqA]
        dc.b $FF


;==========================================
;==========================================
;==========================================

intrmPlyr subroutine

basFreqsPtr     equ W1
basSequencesPtr equ W2
basSeqMapPtr    equ W3
basBeatCt       equ S0
basDur          equ S1
basSeqIdx       equ S2
basSeqMapIdx    equ S3
basFreqIdx      equ S4

sopFreqsPtr     equ W4
sopSequencesPtr equ W5
sopSeqMapPtr    equ W6
sopBeatCt       equ S4+1
sopDur          equ S4+2 ; FIXME this is bleeding into other vars in 0pg
sopSeqIdx       equ S4+3
sopSeqMapIdx    equ S4+4
sopFreqIdx      equ S4+5

        store16 basFreqs,basFreqsPtr
        store16 basSequences,basSequencesPtr
        store16 basSeqMap,basSeqMapPtr

        store16 sopFreqs,sopFreqsPtr
        store16 sopSequences,sopSequencesPtr
        store16 sopSeqMap,sopSeqMapPtr

        lda #0
        sta basBeatCt
        sta basDur
        sta basSeqIdx
        sta basSeqMapIdx
        sta sopBeatCt
        sta sopDur
        sta sopSeqIdx
        sta sopSeqMapIdx

;;;;;;;;;;;;;;;;;
.playMain
;;;;;;;;;;;;;;;;;

.playstepBas
        lda basBeatCt
        cmp basDur
        bne .playBas

; loadNextStepBas
        lda #0
        sta basBeatCt ; reset the beat count

        ldy basSeqMapIdx
        lda (basSeqMapPtr),Y ; load the offset of the bas sequence
        cmp #$FF
        bne .1
        rts ; END_OF_MUSIC
.1
        ; derive the correct offset within the sequence table
        clc
        adc basSeqIdx
        tay
        lda (basSequencesPtr),Y ; get the note,duration nybbles

        ; this is like SplitByte except Y,A instead of X,A
        pha
        lsr
        lsr
        lsr
        lsr
        sta basFreqIdx
        pla
        and #$0F ; duration in A; also Z flag is initialized
        sta basDur

        ; cmp #0 ; duration of 0 means advance to next seq (Z flag already set)
        beq .nextBasSeq

        inc basSeqIdx
        jmp .playBas

.nextBasSeq ; setup a new sequence
        inc basSeqMapIdx
        lda #0
        sta basSeqIdx
        jmp .playstepBas

.playBas
        ldy basFreqIdx
        lda (basFreqsPtr),Y ; note offset assumed in Y
        sta voice1
        inc basBeatCt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.playstepSop
        lda sopBeatCt
        cmp sopDur
        bne .playSop

; loadNextStepSop
        lda #0
        sta sopBeatCt ; reset the beat count

        ldy sopSeqMapIdx
        lda (sopSeqMapPtr),Y ; load the offset of the sop sequence
        cmp #$FF
        bne .2
        rts ; END_OF_MUSIC
.2
        ; derive the correct offset within the sequence table
        clc
        adc sopSeqIdx
        tay
        lda (sopSequencesPtr),Y ; get the note,duration nybbles

        ; this is like SplitByte except Y,A instead of X,A
        pha
        lsr
        lsr
        lsr
        lsr
        sta sopFreqIdx
        pla
        and #$0F ; duration in A; also Z flag is initialized
        sta sopDur

        ; cmp #0 ; duration of 0 means advance to next seq (Z flag already set)
        beq .nextSopSeq

        inc sopSeqIdx
        jmp .playSop

.nextSopSeq ; setup a new sequence
        inc sopSeqMapIdx
        lda #0
        sta sopSeqIdx
        jmp .playstepSop

.playSop
        ldy sopFreqIdx
        lda (sopFreqsPtr),Y ; note offset assumed in Y
        sta voice3
        inc sopBeatCt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
        lda #1
        jsr WaitTime_

        jmp .playMain


