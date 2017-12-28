; intermission music

; note frequency tables
sopFreqs
        dc.b    0 ; (rest)
        dc.b    195; c
        dc.b    201; d
        dc.b    207; e
        dc.b    209; f
        dc.b    215; g
        dc.b    217; aF
        dc.b    219; a
        dc.b    221; bF
        dc.b    223; b
basFreqs
        dc.b    0 ; (rest)
        dc.b    203; e   e-flat
        dc.b    207; E   e-nat
        dc.b    209; f
        dc.b    219; a
        dc.b    221; b   b-flat
        dc.b    223; B   b-nat
        dc.b    225; c
        dc.b    232; F   f oct high
        
; song sequence tables
; byte&f0 --> note table lookup
; byte&0f --> duration; 0 marks End Of Sequence

basSequences
basSeqA
        dc.b [3<<4] + 4 	; f4
        dc.b [0<<4] + 8 	; .8
        dc.b [3<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1
        dc.b [3<<4] + 4 	; f4
        dc.b [0<<4] + 8 	; .8
        dc.b [3<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1

basSeqB
        dc.b [3<<4] + 4 	; f4
        dc.b [0<<4] + 8 	; .8
        dc.b [3<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 3 	; a3
        dc.b [0<<4] + 1 	; .1
        dc.b [5<<4] + 3 	; b3
        dc.b [0<<4] + 1 	; .1
        dc.b [6<<4] + 3 	; B3
        dc.b [0<<4] + 1 	; .1
        dc.b [7<<4] + 3 	; c3
        dc.b [0<<4] + 1 	; .1
        dc.b 0                  ; ..

basSeqCD
        dc.b [8<<4] + 7 	; F7
        dc.b [0<<4] + 1 	; .1
        dc.b [7<<4] + 3 	; c3
        dc.b [0<<4] + 1 	; .1
        dc.b [5<<4] + 3 	; b3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 3 	; a3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 3 	; a3
        dc.b [0<<4] + 1 	; .1
        dc.b [3<<4] + 7 	; f7
        dc.b [0<<4] + 1 	; .1
        dc.b [1<<4] + 7 	; e7
        dc.b [0<<4] + 1 	; .1
        dc.b [2<<4] + 7 	; E7
        dc.b [0<<4] + 1 	; .1
        dc.b [3<<4] + 8 	; f8
        dc.b [0<<4] + 8 	; .8
        dc.b 0                  ; ..

sopSequences
sopSeqA
        dc.b [3<<4] + 1 	; e1
        dc.b [4<<4] + 7 	; f7
        dc.b [3<<4] + 1 	; e1
        dc.b [4<<4] + 7 	; f7
        dc.b [3<<4] + 1 	; e1
        dc.b [4<<4] + 7 	; f7
        dc.b [1<<4] + 1 	; c1
        dc.b [2<<4] + 3 	; d3
        dc.b [1<<4] + 4 	; c4
        dc.b 0                  ; ..

sopSeqB
        dc.b [4<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 8 	; f8
        dc.b [6<<4] + 1 	; G1
        dc.b [7<<4] + 11 	; a11
        dc.b [0<<4] + 8 	; .8
        dc.b 0                  ; ..

sopSeqC
        dc.b [4<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 8 	; f8
        dc.b [1<<4] + 1 	; c1
        dc.b [2<<4] + 11 	; d11
        dc.b [0<<4] + 8 	; .8
        dc.b 0                  ; ..

sopSeqDEF
        dc.b [4<<4] + 3 	; f3
        dc.b [0<<4] + 1 	; .1
        dc.b [4<<4] + 8 	; f8
        dc.b [5<<4] + 1 	; g1
        dc.b [6<<4] + 7 	; G7
        dc.b [8<<4] + 7 	; b7
        dc.b [0<<4] + 1 	; .1
        dc.b [9<<4] + 11 	; B11
        dc.b [0<<4] + 1 	; .1
        dc.b [6<<4] + 1 	; G1
        dc.b [8<<4] + 6 	; b6
        dc.b [0<<4] + 1 	; .1
        dc.b [5<<4] + 1 	; g1
        dc.b [6<<4] + 7 	; G7
        dc.b [4<<4] + 8 	; f8
        dc.b [5<<4] + 1 	; g1
        dc.b [6<<4] + 14 	; G14
        dc.b [0<<4] + 1 	; .1
        dc.b [3<<4] + 1 	; e1
        dc.b [4<<4] + 7 	; f7
        dc.b [0<<4] + 8 	; .8
        dc.b 0                  ; ..

basSeqMap
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
        dc.b $FF


;==========================================
;==========================================
;==========================================

intrmPlyr subroutine
        
basBeatCt       equ S0
basDur          equ S1
basSeqIdx       equ S2
basSeqMapIdx    equ S3
sopBeatCt       equ S4
sopDur          equ W1 ; only need 1 byte of this
sopSeqIdx       equ W1+1
sopSeqMapIdx    equ W2 ; only need 1 byte of this

        lda #0
        sta basBeatCt
        sta basDur
        sta basSeqIdx
        sta basSeqMapIdx
        sta sopBeatCt
        sta sopDur
        sta sopSeqIdx
        sta sopSeqMapIdx

        store16 basSeqMap,W3
        store16 sopSeqMap,W4
        store16 basSequences,W5
        store16 basFreqs,W6

.playstepBas
        lda basBeatCt
        cmp basDur
        bne .playBas
        
; loadNextStepBas
        ldy basSeqMapIdx
        lda (W3),Y ; load the offset of the basesequence
        cmp #$FF
        bne .1
        rts ; END_OF_MUSIC
.1
        ; derive the correct offset within the sequence table
        clc
        adc basSeqIdx
        tay
        lda (W5),Y ; get the note,duration nybbles

        ; this is like SplitByte except Y,A instead of X,A
        pha
        lsr
        lsr
        lsr
        lsr
        tay ; note offset in Y
        pla
        and #$0F ; duration in A; also Z flag is initialized

        ; cmp #0 ; duration of 0 means advance to next seq (Z flag already set)
        beq .nextBasSeq
        
        sta basDur
        lda #0
        sta basBeatCt ; reset the beat count
        inc basSeqIdx
        jmp .playBas

.nextBasSeq ; setup a new sequence
        inc basSeqMapIdx
        lda #0
        sta basSeqIdx
        sta basBeatCt ; reset the beat count
        jmp .playstepBas

.playBas
        lda (W6),Y ; note offset assumed in Y
        sta voice1
        inc basBeatCt

;;
;;
        lda #4
        jsr WaitTime_
        jmp .playstepBas

