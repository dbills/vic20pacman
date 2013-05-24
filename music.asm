;
; Music/sound effect tracks
;
; scale: 0,start freq,end freq,step,dur
; note: 1 freq,dur
        ;;  this sound like the 'waka' when played on the noise channel
Track1                          ; a siren ( up,down scale )
        
    ds 1,0                      ; up scale
    ds 1,218
    ds 1,242
    ds 1,3                    ; step
    ds 1,1                      ; duration

        ds 1,1
        ds 1,0
        ds 1,2
        
    ds 1,0                      ; up scale
    ds 1,242
    ds 1,218
    ds 1,-3                   ; step
    ds 1,1                      ; duration


        ds 1,1
        ds 1,0
        ds 1,2
                                ;    ds 1,2                      ; stop command
        ds 1,3                  ;repeat
Track1x                          ; a siren ( up,down scale )
        
    ds 1,0                      ; up scale
    ds 1,210
    ds 1,240
    ds 1,1                    ; step
    ds 1,1                      ; duration

        ds 1,3
Vol1

        ds 1,1
        ds 1,0
        ds 1,1


        ds 1,1
        ds 1,4
        ds 1,1
        
        ds 1,1
        ds 1,2
        ds 1,1
        
        ds 1,4
        

Track3                          ; a siren ( up,down scale )
    ds 1,0                      ; up scale
    ds 1,200
    ds 1,240
    ds 1,1                      ; step
    ds 1,1                      ; duration
    ds 1,0                      ; up scale
    ds 1,210
    ds 1,240
    ds 1,1            ; step
    ds 1,2            ; duration
    ds 1,2            ; stop command
Track2
    ds 1,1
    ds 1,200
    ds 1,50
    ds 1,1
    ds 1,220
    ds 1,50
    ds 1,1
    ds 1,0
    ds 1,50
    ds 1,2            ; repeat
Track21
    ds 1,1
    ds 1,200
    ds 1,1
    ds 1,1
    ds 1,240
    ds 1,1
    ds 1,2            ; repeat
#if 0        
TrackBass
	ds 1,1
	ds 1,214
	ds 1,240
	ds 1,1
	ds 1,218
	ds 1,120
	ds 1,1
	ds 1,222
	ds 1,255
	ds 1,1
	ds 1,222
	ds 1,105
	ds 1,1
	ds 1,224
	ds 1,255
	ds 1,1
	ds 1,224
	ds 1,105
	ds 1,1
	ds 1,222
	ds 1,255
	ds 1,1
	ds 1,222
	ds 1,105
	ds 1,1
	ds 1,218
	ds 1,255
	ds 1,1
	ds 1,218
	ds 1,105
	ds 1,1
	ds 1,214
	ds 1,255
	ds 1,1
	ds 1,214
	ds 1,105
	ds 1,1
	ds 1,227
	ds 1,120
	ds 1,1
	ds 1,222
	ds 1,120
	ds 1,1
	ds 1,214
	ds 1,120
	ds 1,1
	ds 1,227
	ds 1,120
	ds 1,1
	ds 1,200
	ds 1,60
	ds 1,1
	ds 1,224
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,60
	ds 1,1
	ds 1,218
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,120
	ds 1,1
	ds 1,0
	ds 1,60
        ds 1,2
TrackHigh
	ds 1,1
	ds 1,227
	ds 1,120
	ds 1,1
	ds 1,214
	ds 1,60
	ds 1,1
	ds 1,218
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,60
	ds 1,1
	ds 1,224
	ds 1,60
	ds 1,1
	ds 1,227
	ds 1,120
	ds 1,1
	ds 1,214
	ds 1,90
	ds 1,1
	ds 1,0
	ds 1,30
	ds 1,1
	ds 1,214
	ds 1,90
	ds 1,1
	ds 1,0
	ds 1,30
	ds 1,1
	ds 1,231
	ds 1,120
	ds 1,1
	ds 1,224
	ds 1,60
	ds 1,1
	ds 1,227
	ds 1,60
	ds 1,1
	ds 1,231
	ds 1,60
	ds 1,1
	ds 1,233
	ds 1,60
	ds 1,1
	ds 1,234
	ds 1,120
	ds 1,1
	ds 1,214
	ds 1,90
	ds 1,1
	ds 1,0
	ds 1,30
	ds 1,1
	ds 1,214
	ds 1,90
	ds 1,1
	ds 1,0
	ds 1,30
	ds 1,1
	ds 1,224
	ds 1,120
	ds 1,1
	ds 1,227
	ds 1,60
	ds 1,1
	ds 1,224
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,60
	ds 1,1
	ds 1,218
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,120
	ds 1,1
	ds 1,224
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,60
	ds 1,1
	ds 1,218
	ds 1,60
	ds 1,1
	ds 1,214
	ds 1,60
	ds 1,1
	ds 1,211
	ds 1,120
	ds 1,1
	ds 1,214
	ds 1,60
	ds 1,1
	ds 1,218
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,60
	ds 1,1
	ds 1,214
	ds 1,60
	ds 1,1
	ds 1,222
	ds 1,120
	ds 1,1
	ds 1,218
	ds 1,120
	ds 1,1
	ds 1,0
	ds 1,255
	ds 1,1
	ds 1,0
	ds 1,45
        ds 1,2
#endif