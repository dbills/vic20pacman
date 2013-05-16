;------------------------------------
;
; class Scale &  Note
;    plays a Scale or a note on a VoiceTrack
;    4 instances for the 4 voices
;    if playing a note, then Scale_ef and Scale_st is not relevant
;------------------------------------
Scale_sf      ds 4,0,0,0,0        ; current start freq
Scale_ef      ds 4,0,0,0,0        ; current end freq
Scale_st      ds 4,0,0,0,0        ; step ( signed byte )
Note_dur
Scale_dur     ds 4,0,0,0,0        ; duration in jiffys
Note_rem
Scale_rem     ds 4,0,0,0,0        ; remaining jiffys in current sound
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
        lda #1                      ;tell voicetrack we are done
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
        lda #1                      ;tell voicetrack we are done
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
VoiceTrack_data ds.w    4,0,0,0,0      ; pointer to track data
VoiceTrack_done ds      4,1,1,1,1      ;true when last command is done
VoiceTrack_st   ds.w    4,0,0,0,0      ;address beginning
;
; Called regularly by main loop or VBI
; to service the voice track
; X = voice track to service
;
VoiceTrack_svc SUBROUTINE
        lda #1                      ; IF last command done
        cmp VoiceTrack_done,X
        beq .load_next_command
        ;; call appropriate service routine
        lda Scale_sf,X
        cmp Scale_ef,X
        beq Scale_service       ;sf,ef same, it's a simple note
        bne Scale_service2
        brk                         ; service routine shoulda called rts

.loadscale
        inc16 W1
        lda (W1),Y
        sta Scale_sf,X
        inc16 W1
        lda (W1),Y
        sta Scale_ef,X
        inc16 W1
        lda (W1),Y
        sta Scale_st,X
        inc16 W1
        lda (W1),Y
        sta Scale_dur,X
        inc16 W1
                                ; install Scale as service handler
        move16x2 W1,VoiceTrack_data

        lda #0                      ; set done = false
        sta VoiceTrack_done,X       ;

        jmp Scale_service2
        
.load_next_command
        ;; move pointer to zero page so we can use it for indexing 
        move16x VoiceTrack_data,W1
        ldy #0
        lda (W1),Y
        ;; switch ( track[idx] )
        beq .loadscale
        cmp #1                  ; case 1
        beq  .load_note         ;
        cmp #3
        beq .repeat
        cmp #02                 ; case 2 stop command
        brk
        rts
        
.repeat
        move16x    VoiceTrack_st,W1
        jmp .loadscale
        
.load_note
        inc16 W1
        lda (W1),Y                  ; get note freq
        ;; we use the same start/end freq for 'notes'
        sta Scale_sf,X              ; save start freq
        sta Scale_ef,X              ; save end freq
        bne .0
        lda #0
        sta volume
.0
        lda #8
        sta volume
        
        lda #1                      ; direction = up ( not really relevant )
        sta Scale_st,X
        inc16 W1
        lda (W1),Y                  ; get note dur
        sta Note_dur,X              ;
        sta Note_rem,X              ; initialize remaining time
        inc16 W1
                                ; install Scale as service handler
        move16x2 W1,VoiceTrack_data
        
        lda #0                      ; set done = false
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