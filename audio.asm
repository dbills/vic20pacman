
DeathSound
        dc.b 2                  ;frame 1
        dc.b 233
        dc.b 234
        dc.b 235
        dc.b 236
        dc.b 237
        dc.b 238
        dc.b 239
        dc.b 236
        dc.b 2                  ;frame 2
        dc.b 233
        dc.b 230
        dc.b 231
        dc.b 232
        dc.b 233
        dc.b 234
        dc.b 235
        dc.b 236
        dc.b 2                  ;frame 3
        dc.b 233
        dc.b 230
        dc.b 227
        dc.b 228
        dc.b 229
        dc.b 230
        dc.b 231
        dc.b 232
        dc.b 2                  ;frame 4
        dc.b 233
        dc.b 230
        dc.b 227
        dc.b 224
        dc.b 225
        dc.b 226
        dc.b 227
        dc.b 228
        dc.b 2                  ;frame 5
        dc.b 229
        dc.b 230
        dc.b 227
        dc.b 224
        dc.b 221
        dc.b 222
        dc.b 223
        dc.b 224
        dc.b 225
        dc.b 226
        dc.b 227
        dc.b 224
        dc.b 221
        dc.b 2                  ;frame 6
        dc.b 128
        dc.b 143
        dc.b 157
        dc.b 170
        dc.b 182
        dc.b 193
        dc.b 203
        dc.b 212
        dc.b 220
        dc.b 2                  ;frame 7
        dc.b 128
        dc.b 143
        dc.b 157
        dc.b 170
        dc.b 182
        dc.b 193
        dc.b 203
        dc.b 212
        dc.b 220
        dc.b 0     
FruitSound
        dc.b 248                ;unused due to sloppy,but. byte saving code in game
        dc.b 248
        dc.b 247
        dc.b 246
        dc.b 245
        dc.b 244
        dc.b 243
        dc.b 241
        dc.b 239
        dc.b 237
        dc.b 234
        dc.b 230
        dc.b 226
        dc.b 230
        dc.b 234
        dc.b 237
        dc.b 239
        dc.b 241
        dc.b 243
        dc.b 244
        dc.b 245
        dc.b 246
        dc.b 247
        dc.b 248
        dc.b 0
; note frequency tables
sop1
        dc.b 0
        dc.b 224
        dc.b 231
        dc.b 234
        dc.b 240
        dc.b 232
        dc.b 236
        dc.b 237
        dc.b 238
        dc.b 239
sop2
        dc.b 0
        dc.b 226
        dc.b 232
        dc.b 236
        dc.b 241
bas1
        dc.b 0
        dc.b 192
        dc.b 214
        dc.b 218
        dc.b 222
        dc.b 224
bas2
        dc.b 0
        dc.b 197
        dc.b 216

; song sequence tables
; byte&f0 --> bass
; byte&0f --> soprano
seq1
        dc.b [1<<4] + 1
        dc.b [1<<4] + 0
        dc.b [0<<4] + 4
        dc.b [0<<4] + 0
        dc.b [0<<4] + 3
        dc.b [0<<4] + 0
        dc.b [2<<4] + 2
        dc.b [2<<4] + 0
        dc.b [1<<4] + 4
        dc.b [1<<4] + 3
        dc.b [0<<4] + 0
        dc.b [0<<4] + 0
        dc.b [0<<4] + 2
        dc.b [0<<4] + 2
        dc.b [2<<4] + 0
        dc.b [0<<4] + 0
        dc.b $ff
seq2
        dc.b [2<<4] + 2
        dc.b [2<<4] + 5
        dc.b [0<<4] + 3
        dc.b [0<<4] + 0
        dc.b [3<<4] + 3
        dc.b [3<<4] + 6
        dc.b [0<<4] + 7
        dc.b [0<<4] + 0
        dc.b [4<<4] + 7
        dc.b [4<<4] + 8
        dc.b [0<<4] + 9
        dc.b [0<<4] + 0
        dc.b [5<<4] + 4
        dc.b [5<<4] + 4
        dc.b [0<<4] + 0
        dc.b $ff


;; Measure 1:
;; seq1 + sop1,bas1
;; Measure 2:
;; seq1 + sop2,bas2
;; Measure 3:
;; seq1 + sop1,bas1
;; Measure 4:
;; seq2 + sop1,bas1


SopFreqPtr     equ W2         ; pointer to active note freq table
BasFreqPtr     equ W3         ; pointer to active base note freq table
SequencePtr    equ S0         ; pointer to active sequence
SequenceIdx    equ Audio1

player subroutine
        store16 sop1,SopFreqPtr
        store16 bas1,BasFreqPtr
        store16 seq1,SequencePtr
        jsr PlayMeasure
        
        store16 sop2,SopFreqPtr
        store16 bas2,BasFreqPtr
        
        jsr PlayMeasure

        store16 sop1,SopFreqPtr
        store16 bas1,BasFreqPtr
        jsr PlayMeasure
        
        store16 seq2,SequencePtr  
;        jsr ShowActors
        jsr PlayMeasure

        rts

PlayMeasure subroutine
        lda #0
        sta SequenceIdx
.loop   
        ldy SequenceIdx
        lda (SequencePtr),Y
        bmi .done
        jsr SplitByte        ;a=low x=high
        tay
        lda (SopFreqPtr),Y
        sta  voice3
        txa                  ; x ->a
        tay                  ; a ->y
        lda (BasFreqPtr),Y
        sta  voice1          ; store base freq

        lda #4
        jsr WaitTime_
        
        inc SequenceIdx
        bne .loop
.done
	 rts

