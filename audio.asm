;------------------------------------
;
; class Scale &  Note
;    plays a Scale or a note on a VoiceTrack
;    4 instances for the 4 voices
;    if playing a note, then Scale_ef and Scale_st is not relevant
;------------------------------------
Scale_sf      ds 5,0,0,0,0,0        ; current start freq
Scale_ef      ds 5,0,0,0,0,0       ; current end freq
Scale_st      ds 5,0,0,0,0,0        ; step ( signed byte )
Note_dur
Scale_dur     ds 5,0,0,0,0,0        ; duration in jiffys
Note_rem
Scale_rem     ds 5,0,0,0,0,0        ; remaining jiffys in current sound
;--------
; methods
;--------
;
; called every VoiceTrack, figures out the next
; note to play and when, loads it into the sound
; register
; X = voicetrack we are on
;
Scale_service SUBROUTINE
        lda Scale_sf,X              ; get current note
        sta voice1,X                ; play it



        dec Scale_rem,X             ; decrement remaining note time
        beq .done
;        dec Scale_rem,X             ; decrement remaining note time
;       beq .done
        rts
.done
        lda #trackDone          ;tell voicetrack we are done
        sta VoiceTrack_done,X
        rts
Scale_service2 SUBROUTINE
        lda Scale_sf,X              ; get current note
        sta voice1,X                ; play it

        clc
        adc Scale_st,X
        sta Scale_sf,X
        cmp Scale_ef,X
        beq .done
        rts
.done
        lda #trackDone                      ;tell voicetrack we are done
        sta VoiceTrack_done,X
        rts
;-------------------------------------------
; end class Scale
;-------------------------------------------

;-------------------------------------------
; class VoiceTrack
;    manages a series of Commands for a
;    voice
;    e.g. Scale,Note ( see above )
;;; voice track data commands
;;; 0 = scale
;;; [cmd][note][duration]
;;; 1 = note
;;; not doc'd yet
;;; 2 = repeat
;-------------------------------------------
trackDone     equ 0
trackRunning  equ 1        
VoiceTrack_data ds.w    5,0,0,0,0,0      ; pointer to track data
VoiceTrack_done ds      5,trackDone,trackDone,trackDone,trackDone,trackDone
VoiceTrack_st   ds.w    5,0,0,0,0,0      ;address beginning
VoiceTrack_halted ds    5,1,1,1,1,1
;
; Called regularly by main loop or VBI
; to service the voice track
; X = voice track to service
;
VoiceTrack_svc SUBROUTINE
        lda VoiceTrack_halted,x
        bne .nothalted

        rts
.nothalted
        lda VoiceTrack_done,X
        beq .load_next_command
        ;; call appropriate service routine
        lda Scale_sf,X
        cmp Scale_ef,X
        beq Scale_service       ;sf,ef same, it's a simple note
        bne Scale_service2
        brk                         ; service routine shoulda called rts

.loadscale
        inc16 AUDIO
        lda (AUDIO),Y
        sta Scale_sf,X
        inc16 AUDIO
        lda (AUDIO),Y
        sta Scale_ef,X
        inc16 AUDIO
        lda (AUDIO),Y
        sta Scale_st,X
        inc16 AUDIO
        lda (AUDIO),Y
        sta Scale_dur,X
        inc16 AUDIO
                                ; install Scale as service handler
        move16x2 AUDIO,VoiceTrack_data

        lda #trackRunning 
        sta VoiceTrack_done,X       ;
        jmp Scale_service2  
        
.load_next_command
        ;; move pointer to zero page so we can use it for indexing 
        move16x VoiceTrack_data,AUDIO
        ldy #0
        lda (AUDIO),Y
        ;; switch ( track[idx] )
        beq .loadscale
        cmp #1                  ; case 1
        beq  .load_note         ;
        cmp #3
        beq .repeat
        cmp #4
        beq .repeat2
        brk
.done        
        rts
        
.repeat
        move16x    VoiceTrack_st,AUDIO
        jmp .loadscale
.repeat2        
        move16x    VoiceTrack_st,AUDIO
        jmp .load_note
        
.load_note
        inc16 AUDIO
        lda (AUDIO),Y                  ; get note freq
        ;; we use the same start/end freq for 'notes'
        sta Scale_sf,X              ; save start freq
        sta Scale_ef,X              ; save end freq
        lda #1                      ; direction = up ( not really relevant )
        sta Scale_st,X
        inc16 AUDIO
        lda (AUDIO),Y                  ; get note dur
        sta Note_dur,X              ;
        sta Note_rem,X              ; initialize remaining time
        inc16 AUDIO
                                ; install Scale as service handler
        move16x2 AUDIO,VoiceTrack_data
        
        lda #trackRunning
        sta VoiceTrack_done,X       ;
        jmp Scale_service           ;
        brk                         ; shouldn't get here


;-------------------------------------------
; end class VoiceTrack
;-------------------------------------------
        ;; loadtrack voice,track
        MAC LoadTrack
        ldx #{1}
        store16x {2},VoiceTrack_data
        store16x {2},VoiceTrack_st
        ENDM

        ;; X track to halt
        MAC HaltTrack
        lda #trackDone
        sta VoiceTrack_halted,X
        lda #0
        sta 36877
        ENDM
        ;; X track
        MAC UnHaltTrack
        lda #trackRunning
        sta VoiceTrack_halted,X
        ENDM