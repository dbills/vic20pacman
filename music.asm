;
; Music/sound effect tracks
;
; scale: 0,start freq,end freq,step,dur
; note: 1 freq,dur

Track1                          ; a siren ( up,down scale )
        
    ds 1,0                      ; up scale
    ds 1,220
    ds 1,240
    ds 1,2                      ; step
    ds 1,1                      ; duration

        ds 1,1
        ds 1,0
        ds 1,5
        
    ds 1,0                      ; up scale
    ds 1,240
    ds 1,220
    ds 1,-2                     ; step
    ds 1,1                      ; duration
;    ds 1,2                      ; stop command
        ds 1,3                  ;repeat

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
table1                ; multiple of 22 table for screen
    ds 1,44
    ds 1,66
    ds 1,88
    ds 1,110
    ds 1,132
    ds 1,154
    ds 1,176
    ds 1,198
    ds 1,220
    ds 1,242
    ds 1,264
    ds 1,286
    ds 1,308
    ds 1,330
    ds 1,352
    ds 1,374
    ds 1,396
    ds 1,418
    ds 1,440
    ds 1,462
    ds 1,484
#endif
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
