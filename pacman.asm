    org $4000
    processor 6502
_debug    equ 1            ; true for debugging
voice1    equ 36874        ; sound registers
voice2    equ 36875
voice3    equ 36876
volume    equ 36878
screen    equ $1000        ; screen ram
clrram    equ $9400        ; color ram for screen

VIA1DDR equ $9113
VIA2DDR equ $9122          ; ?
JOY0    equ $9111
JOY0B   equ $9120          ; output register B VIA #2
JOYUP   equ $4             ; joy up bit
JOYDWN  equ $8             ; joy down
JOYL    equ $10            ; joy left
JOYT    equ $20            ; joy fire
JOYR    equ $80            ; joy right bit

jiffyh    equ $a0          ; jiffy clock lsb - msb
jiffym    equ $a1
jiffyl    equ $a2

chrom1    equ $8000        ; upper case character rom
chrom2    equ $8400        ; upper case reversed
chrom3    equ $8c00        ; upper lower reversed
chrom4    equ $8800        ; upper lower

;; vic-I chip registers
chrst           equ $9003        ; font map pointer
mychars         equ $1400        ; my font map
charcnt         equ $800

BLACK        equ 0
WHITE        equ 1
RED          equ 2
CYAN         equ 3
PURPLE       equ 4
GREEN        equ 5
BLUE         equ 6
YELLOW       equ 7
ORANGE       equ 8
LTORANGE     equ 9
PINK         equ 10
LTCYAN       equ 11
LTPURPLE     equ 12
LTGREEN      equ 13
LTBLUE       equ 14
LTYELLOW     equ 15

MW           equ $e0            ;maze wall character
SPRITES      equ 2             ;count of sprites in system 1 based
;;
;;  Zero page constants
;;
;;; W prefix vars are WORD width
;;; S or byte, short for 'scratch'
W1              equ 0
W1_l            equ 0
W1_h            equ 1
W2              equ 2
W2_l            equ 2
W2_h            equ 3        
S3              equ 4
S4              equ 5
W4              equ 6           ;2 byte work var
W3              equ $12        ; 16 bit work 3
S1              equ $14        ; 1 byte scratch reg
S2              equ $15        ; 1 byte scratch reg
PACFRAME        equ $16        ; pointer to current pacman bitimage ( changes for animation )
PACFRAMEN       equ $18        ; byte: index of pac frame
PACFRAMEDIR     equ $19        ; the direction pacman is facing
S5              equ $30
S6              equ $31        
W5              equ $32
S7              equ $43
W6              equ $44
VV              equ $02         ;testing, voice 2
;;
;; misc constants
;;
PACL            equ $00            ; pacman char number

GHL             equ $08            ; ghost char number

DOT             equ $04
PWR             equ $C0
;
;
;------------------------------------
; Utility macros
;------------------------------------
;
#if _debug        
   mac checkYDir
        cpy #22
        beq .checkYok
        cpy #1
        beq .checkYok
        brk
.checkYok
   endm
#else        
   mac checkYDir
   endm
#endif        
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
    ;; ABORT instruction
    mac abort
    lda #$c0
    sta $9005
    brk
    endm
    ;; MOVE instruction
    mac mov
    lda [{1}]
    sta [{2}]
    endm
; write 16 bit address to dest
;;; (source,dest)
    mac store16
    lda #[{1}] & $ff    ; load low byte
    sta {2}             ; store low byte
    lda #[{1}] >> 8     ; load high byte
    sta [{2}]+1         ; store high byte
    endm
; load A and X
    mac     ldax
    lda     [{1}]
    ldx     [{1}]+1
    endm
; store A and X
    mac    stax
    sta [{1}]
    stx [{1}]+ 1
    endm

    mac dec16
    LDA [{1}]+0
    BNE .done
    DEC [{1}]+1
.done    DEC [{1}]+0
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
; move16(source,dest),Y
; dest+y = source ( as opposed to dest[y]=source )
    mac move16y
    lda [{1}],Y
    sta [{2}]
    iny
    lda [{1}],Y
    sta [{2}]+1
    endm
    ;; move word index by X, does correct pointer
    ;; arithmetic for 'word'
    ;; move16x source[X],dest
    mac move16x
    txa
    pha
    asl                         ; x*2 since it's a word
    tax
    lda [{1}],X
    sta [{2}]
    inx
    lda [{1}],X
    sta [{2}]+1
    pla
    tax
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
    ;; 16 bit add W1+W2 result in W1
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
;; subtract 16 bit numbers
    mac subxx
        SEC
        LDA [{1}]+0
        SBC [{3}]+0
        STA [{2}]+0
        LDA [{1}]+1
        SBC #0
        STA [{2}]+1
    endm
;;; find the character font address of the tile
;;; underneath a sprite
;;; on entry: A = tile in question
;;; W4 (out) font address of tile underneath
;;; uses S5,S6,W5
        mac mergeTile 
        sta S5
        lda #8
        sta S6
        jsr multxx
        lda W5
        clc
        adc #mychars&$ff
        sta W4
        lda W5+1
        adc #mychars >> 8
        sta W4+1
        endm
;; set N on joystick right
  mac onjoyr
        lda #127
        sta VIA2DDR
        lda JOY0B
        ldx #$ff
        stx VIA2DDR
        and #JOYR
  endm
        ;; test code for multxx routine
        ;; lda #131
        ;; sta S5
        ;; lda #12
        ;; sta S6
        ;; lda #0
        ;; sta W5
        ;; sta W5+1
        ;; jsr multxx
        ;; brk
        lda #0
        sta S7
    jmp main

    INCLUDE "music.asm"        
;;; screen map for maze
Maze1
    HEX  20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
    HEX  20 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0
    HEX  20 E0 04 04 04 04 C0 04 04 04 04 E0 04 04 04 04 04 04 04 04 04 E0
    HEX  20 E0 C0 E0 E0 04 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 04 E0 E0 C0 E0
    HEX  20 E0 04 E0 E0 04 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 04 E0 E0 04 E0
    HEX  20 E0 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 E0
    HEX  20 E0 04 E0 E0 04 E0 04 E0 E0 E0 E0 E0 E0 E0 04 E0 04 E0 E0 04 E0
    HEX  20 E0 04 04 04 04 E0 04 04 04 04 E0 04 04 04 04 E0 04 04 04 04 E0
    HEX  20 E0 E0 E0 E0 04 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 04 E0 E0 E0 E0
    HEX  20 20 20 20 E0 04 E0 20 20 20 20 20 20 20 20 20 E0 04 E0 20 20 20
    HEX  20 E0 E0 E0 E0 04 E0 20 E0 E0 E0 E0 E0 E0 E0 20 E0 04 E0 E0 E0 E0
    HEX  20 20 20 20 21 04 20 20 E0 20 20 20 20 20 E0 20 20 04 20 20 20 20
    HEX  20 E0 E0 E0 E0 04 E0 20 E0 E0 E0 E0 E0 E0 E0 20 E0 04 E0 E0 E0 E0
    HEX  20 20 20 20 E0 04 E0 20 20 20 20 20 20 20 20 20 E0 04 E0 20 20 20
    HEX  20 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0
    HEX  20 E0 25 29 27 28 04 04 04 04 04 E0 04 04 04 04 04 04 04 04 04 E0
    HEX  20 E0 04 E0 E0 04 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 04 E0 E0 04 E0
    HEX  20 E0 04 04 E0 04 04 04 04 04 04 04 04 04 04 04 04 04 E0 04 04 E0
    HEX  20 E0 E0 04 E0 04 E0 04 E0 E0 E0 E0 E0 E0 E0 04 E0 04 E0 04 E0 E0
    HEX  20 E0 C0 04 04 04 E0 04 04 04 04 E0 04 04 04 04 E0 04 04 04 C0 E0
    HEX  20 E0 04 E0 E0 E0 E0 E0 E0 E0 04 E0 04 E0 E0 E0 E0 E0 E0 E0 04 E0
    HEX  20 E0 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 04 E0
    HEX  20 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0 E0
;;; color map for maze
Maze1C
    HEX  01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01
    HEX  01 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06
    HEX  01 06 01 01 01 01 01 01 01 01 01 06 01 01 01 01 01 01 01 01 01 06
    HEX  01 06 08 06 06 01 06 06 06 06 01 06 01 06 06 06 06 01 06 06 08 06
    HEX  01 06 01 06 06 01 06 06 06 06 01 06 01 06 06 06 06 01 06 06 01 06
    HEX  01 06 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 06
    HEX  01 06 01 06 06 01 06 01 06 06 06 06 06 06 06 01 06 01 06 06 01 06
    HEX  01 06 01 01 01 01 06 01 01 01 01 06 01 01 01 01 06 01 01 01 01 06
    HEX  01 06 06 06 06 01 06 06 06 06 01 06 01 06 06 06 06 01 06 06 06 06
    HEX  01 01 01 01 06 01 06 01 01 01 01 01 01 01 01 01 06 01 06 01 01 01
    HEX  01 06 06 06 06 01 06 01 06 06 06 06 06 06 06 01 06 01 06 06 06 06
    HEX  01 01 01 01 01 01 01 01 06 01 01 01 01 01 06 01 01 01 01 01 01 01
    HEX  01 06 06 06 06 01 06 01 06 06 06 06 06 06 06 01 06 01 06 06 06 06
    HEX  01 01 01 01 06 01 06 01 01 01 01 01 01 01 01 01 06 01 06 01 01 01
    HEX  01 06 06 06 06 01 06 01 06 06 06 06 06 06 06 01 06 01 06 06 06 06
    HEX  01 06 01 01 01 01 01 01 01 01 01 06 01 01 01 01 01 01 01 01 01 06
    HEX  01 06 01 06 06 01 06 06 06 06 01 06 01 06 06 06 06 01 06 06 01 06
    HEX  01 06 01 01 06 01 01 01 01 01 01 01 01 01 01 01 01 01 06 01 01 06
    HEX  01 06 06 01 06 01 06 01 06 06 06 06 06 06 06 01 06 01 06 01 06 06
    HEX  01 06 01 01 01 01 06 01 01 01 01 06 01 01 01 01 06 01 01 01 08 06
    HEX  01 06 01 06 06 06 06 06 06 06 01 06 01 06 06 06 06 06 06 06 01 06
    HEX  01 06 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 06
    HEX  01 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06

pacframes  equ #3            ; total number of pacman animation frames
;;; 
;;; define some 8x8 characters
;;; 
#if 1
PAC1                            ; closed
    ds 1,60
    ds 1,126
    ds 1,255
    ds 1,255
    ds 1,255
    ds 1,255
    ds 1,126
    ds 1,60
PAC2
    ds 1,60
    ds 1,126
    ds 1,255
    ds 1,240
    ds 1,240
    ds 1,255
    ds 1,126
    ds 1,60
PAC3
    ds 1,60
    ds 1,126
    ds 1,248
    ds 1,240
    ds 1,240
    ds 1,248
    ds 1,126
    ds 1,60
PAC4                ; wide open
    ds 1,60
    ds 1,126
    ds 1,248
    ds 1,240
    ds 1,240
    ds 1,248
    ds 1,126
    ds 1,60
#else

PAC1 ds 8,$ff
PAC2 ds 8,$ff
PAC3 ds 8,$ff
PAC4 ds 8,$ff
#endif

GHOST
    ds 1,126
    ds 1,195
    ds 1,215
    ds 1,255
    ds 1,255
    ds 1,227
    ds 1,255
    ds 1,170
PPWR
    ds 1,0,
    ds 1,24
    ds 1,60
    ds 1,60
    ds 1,60
    ds 1,60
    ds 1,24
    ds 1,0
;;; the dot graphic that pacman eats
PDOT
    ds 1,0,
    ds 1,0
    ds 1,0
    ds 1,24
    ds 1,24
    ds 1,0
    ds 1,0
    ds 1,0

;------------------------------------
;;;
;;; class Sprite
;;; 5 instances, 4 ghosts, 1 pacman
;;; sprite tile is the 'head tile'
;;; sprite tile + 1 is the tail tile
;;; sprite tile + 2 is the upcoming frame's head tile
;;; sprite tile + 3 is the upcoming frame's tail tile
;;; we 'page flip' the tiles so that the expensive bit rendering
;;; does not have to be done on the vertical blank as my routines
;;; which probably suck, take up too much time, and I was
;;; getting flicker on the first few dozen scan lines because I
;;; was still busy rendering
;;; sprite_page control which set of tiles we draw , it alternates
;;; between 0 and 1
;------------------------------------
Sprite_page     dc.b 0        
Sprite_loc      DC.W screen+22*5+4,screen+22*15+2,0,0,0    ;screen loc
Sprite_loc2     DC.W screen+22*5+4,screen+22*15+2,0,0,0    ;new screen loc
Sprite_back     dc.b 0,0,0,0,0           ;background char value before other sprites are drawn
Sprite_back2    dc.b 0,0,0,0,0           ;static screen background
Sprite_sback    dc.b 0,0,0,0,0              ;current screen background ( might include some other sprite tile that was laid down )
Sprite_sback2   dc.b 0,0,0,0,0        
Sprite_tile     dc.b PACL,GHL,0,0,0         ;foreground char
Sprite_src      dc.w PAC2,GHOST,0,0,0       ;sprite source bitmap
;;; sprite chargen ram ( where to put the source bmap )
Sprite_bmap     dc.w mychars+(PACL*8),      mychars+(GHL*8)      ,0,0,0    
Sprite_bmap2    dc.w mychars+(PACL*8)+(2*8),mychars+(GHL*8)+(16),0,0,0    
Sprite_dir      dc.b 1,1,1,1,1  ;sprite direction 1(horiz),22(vert)
Sprite_dir2     dc.b 1,1,1,1,1  ;sprite direction 1(horiz),22(vert)    
Sprite_offset   dc.b 0,4,0,0,0  ;sprite bit offset in tiles
Sprite_offset2  dc.b 0,4,0,0,0  ;sprite bit offset in tiles
        
;;; S2 sprite to render
render_sprite SUBROUTINE
#if _debug        
        stx S2                  ;set for call to blith
        lda #SPRITES
        cmp S2
        bcs .ok
        brk
.ok
#endif        
        move16x Sprite_src,W1   ;bitmap source -> W1
        ;; assume tile pair 0
        ;; if we are currently on tile pair 0 , then render into pair 1
        move16x Sprite_bmap2,W2  ;left tile chargen ram
        ldy #0
        cpy Sprite_page
        beq .page0
        ;; else currently on tile pair 1, render into page 0
        move16x Sprite_bmap,W2  ;left tile chargen ram
.page0
        addxx W2,W3,#$8          ;right tile chargen ram
        
        lda Sprite_offset2,X
        sta S1                  ;setup for blitd,blith,blitc
        jsr blitc        ;
        lda Sprite_dir2,X
        cmp #1
        beq .horiz
        cmp #22
        beq .vert
        brk
.horiz        
        jsr blith
        rts
.vert
        jsr blitd
        rts
;;; X = sprite to erase
erasesprt SUBROUTINE
        move16x Sprite_loc,W1   ;sprite location to W1
        ldy #0
        lda Sprite_back,X
        bne .ok0
        brk
.ok0        
        sta (W1),Y
        
        ldy Sprite_dir,X
        checkYDir
        lda Sprite_back2,X
        bne .ok1
        brk
.ok1        
        sta (W1),Y              ;save char under right tile

        clc
        lda #$84                ;W1 now = color ram location
        adc W1+1
        sta W1+1
        lda #WHITE
        ldy #0
        sta (W1),Y
        ldy Sprite_dir,X
        sta (W1),Y
        rts
;;; X = sprite to move
drwsprt1 SUBROUTINE
        move16x Sprite_loc,W1
        ldy #0
        lda (W1),Y
        sta Sprite_sback,X      ;current screen backing tile
        lda Sprite_tile,X
        ;; figure out which set of tiles we should be rendering
        ;; we 'page flip' between tiles for speed
        cpy Sprite_page
        beq .page0
        clc
        adc #2
.page0        
        sta (W1),Y
        clc
        adc #01
        sta S1                  ;save right tile character
        ldy Sprite_dir,X
        lda (W1),Y
        sta Sprite_sback2,X     ;current screen backing tile(2)
        lda S1                  ;load right tile character
        sta (W1),Y
        clc
        lda #$84                ;W1 now = color ram location
        adc W1+1
        sta W1+1
        lda #YELLOW
        ldy #0
        sta (W1),Y
        ldy Sprite_dir,X
        sta (W1),Y
        rts

 INCLUDE "audio.asm"
;-------------------------------------------
; MAIN()
;-------------------------------------------
main SUBROUTINE
    cli                         ; enable interrupts so jiffy clock works
    lda #8
    sta 36879                   ; border and screen colors
    lda #$0f                    ; max
    sta volume                  ; turn up the volume
    lda #0
    sta $9113                   ; joy VIA to input
    jsr copychar                ; copy our custom char set
    store16 PAC4,W1             ; copy(pacman,character[0])
    store16 mychars,W2
    jsr loadch                  ;
    store16 PDOT,W1             ; copy(dot,character[1]);
    store16 mychars+[8*DOT] ,W2 ;
    jsr loadch                  ;
    store16 PPWR,W1             ; copy(power,charcters[2]);
    store16 mychars+[8*PWR],W2  ;
    jsr loadch                  ;

    jsr mkmaze

    ldx #1                      ; turn on voice track
    store16x TrackBass,VoiceTrack_data
    store16x TrackBass,VoiceTrack_st
    ldx #2                      ; voice 3
    store16x TrackHigh,VoiceTrack_data
    store16x TrackHigh,VoiceTrack_st
;    ldx #1
;    store16x Track1,VoiceTrack_data    ; load track 1 on voice 2

    jmp .background
.loop
        ldx #1
.iloop        
        lda $9004
        beq .2
        bne .iloop

.2
        dex
        bne .iloop
        lda #1
        eor Sprite_page
        sta Sprite_page
;    ldx #1                     ; service voice VV
;    jsr VoiceTrack_svc         ; run sound engine
;    ldx #2
;    jsr VoiceTrack_svc          ; run sound engine
        lda #SPRITES
        sta S3
.eraseloop
        dec S3
        bmi .background
        ldx S3
        jsr erasesprt
        jmp .eraseloop

.background
        lda #SPRITES
        sta S3
.backloop        
        dec S3
        bmi .draw
        ldx S3
        ;; swap the new position to the current
        move16xx Sprite_loc2,Sprite_loc
        lda Sprite_dir2,X
        sta Sprite_dir,X
        lda Sprite_offset2,X
        sta Sprite_offset,X
        ;; collect background tiles so we can replace them later
        move16x Sprite_loc,W1
        ldy #0
        lda (W1),Y
#if _debug        
        bne .ok0
        brk
.ok0
#endif        
        sta Sprite_back,X
        ldy Sprite_dir,X
        checkYDir
        lda (W1),Y
        sta Sprite_back2,X
        ;; end collection of background tiles
        jmp .backloop
        
.draw
        ;brk
        lda #SPRITES
        sta S3
.drawloop
        dec S3
        bmi .player
        ldx S3
#if _debug        
        jsr dumpBack
#endif        
        jsr drwsprt1             ;draw in new location
        jmp .drawloop
.player
;;; this is the beginning of non-time critical stuff
;;; everything before this, we hoped had been completed on the vertical blank
        jsr Pacman
        jsr Ghost
        lda #SPRITES
        sta S3
.playerloop
        dec S3
        bmi .loopend
        ldx S3
        jsr render_sprite
        jmp .playerloop

.loopend

        jmp .loop
        brk
;;; -------------------------------------------------------------------------------------------
;;; load a character image
;;; W1 = source data
;;; W2 = dest data
loadch SUBROUTINE
    ldy #0
.1  lda (W1),y                  ;lda (*w1)+y
    sta (W2),y
    iny
    cpy #8
    beq .done
    jmp .1
.done
    rts
;;;
;;;  copy the stock character set
;;;
copychar    SUBROUTINE
    store16 chrom1 ,W2
    store16 mychars,W3
    store16 charcnt,W1_l

    jsr movedown

    lda $9005
    and #$f0
    ora #$0d
    sta $9005

    rts

mkmaze SUBROUTINE
    store16 Maze1,W2
    store16 screen,W3
    lda #$fa
    sta W1_l
    lda #$01
    sta W1_h
    jsr movedown
    store16 Maze1C,W2
    store16 clrram,W3
    jsr movedown
    rts
; Move memory down
;
; W2 = source start address
;   W3 = destination start address
; SIZE = number of bytes to move
;
movedown SUBROUTINE
        LDY #0
        LDX W1_h
        BEQ md2
md1
        LDA ( W2 ),Y ; move a page at a time
        STA ( W3 ),Y
        INY
        BNE md1
        INC W2+1
        INC W3+1
        DEX
        BNE md1
md2
        LDX W1_l
        BEQ md4
md3
        LDA ( W2 ),Y ; move the remaining bytes
        STA ( W3 ),Y
        INY
        DEX
        BNE md3
md4
        RTS

;;; animate a ghost back and forth
Ghost SUBROUTINE

        ldx #1
#if 1
;;; Service ghosts
        lda S7
        beq .sr
        jsr scroll_left        ;
        bcs .reverse2
        bcc .0
.reverse2
        lda #0
        sta S7
        jmp .0
.sr
        jsr scroll_right        ;
        bcs .reverse
        bcc .0
.reverse
        lda #1
        sta S7                ;
#endif
.0
        rts
;;; 
;;; Service PACMAN, read joystick and move
;;; 
Pacman SUBROUTINE
        ldx #0                  ;work with sprite 0
        lda JOY0                ; read joy register
        sta $1001               ; debug char on screen
        tay                   
        and #JOYL               ; check for left bit
        beq .left
        tya                     ; restore original read
        and #JOYUP              ; check for up bit
        beq .up
        tya                     ; restore original read
        and #JOYDWN             ; check for down bit
        beq .down
        tya
        and #JOYT
        beq .fire
        tya
        onjoyr
        beq .right
        rts
.down                           ;
        jsr scroll_down
        rts
.right
        ldx #0
        jsr scroll_right
        rts
.left
        jsr scroll_left
        rts
.up
        jsr scroll_up
        rts
.fire
        brk
        rts
;;;
;;; move sprite  right
;;; X = sprite to move
scroll_right SUBROUTINE
        lda Sprite_dir,X
        cmp #1
        beq .ok
        lda #24
        sta S1
        jsr changehoriz
        bcc .ok
        rts
.ok
        ldy Sprite_offset,X
        cpy #8
        beq .course             ; exhauted smooth scroll?
        iny                     ; increment smooth scroll
        bne .draw
.course                         ;course scroll
        move16x Sprite_loc,W2;        brk
        addxx W2,W1,#$02        ;check 2 places to the right for wall
        jsr move_ok
        bne .00
        rts
.00
        ldy #0
        inc16 W2
        move16x2 W2,Sprite_loc2
        iny                     ;reset scroll offset to 1
.draw
        tya                     ;note, if we just course scrolled, we've already set Y = 0
        sta Sprite_offset2,X
.done
        clc
        rts

;;;
;;; move sprite  
;;; X = sprite to move
scroll_left SUBROUTINE
        lda Sprite_dir,X
        cmp #1
        beq .ok                 ;ok to move horizontal
        ;; switch to horiz order
        lda #22
        sta S1
        jsr changehoriz
        bcc .ok
        rts
.ok
        ldy Sprite_offset,X
        beq .course             ; exhauted smooth scroll?
        bne .draw
.course                         ;course scroll
        move16x Sprite_loc,W2   ;
        dec16 W2                ;check to the left for wall
        lda #MW
        ldy #0
        cmp (W2),Y
        sec
        beq .done               ; we hit a wall
        
        move16x2 W2,Sprite_loc2  ;save the new sprite screen location
        ldy #8                   ;reset sprite offset to 8
.draw
        dey                     ; increment smooth scroll
        tya
        sta Sprite_offset2,X
        clc
.done
        rts
;;
;; set zero flag if move is forbidden
;; proposed pacman leftmost position is in W1
;;
move_ok SUBROUTINE
        ldy #0
        lda (W1),y
        cmp #MW            ; is it a wall?
        rts
;;; clear all bits to 0
;;; W2 = top half font ram
;;; W3 = bottom half font ram
blitc SUBROUTINE
    ldy #8
    lda #0
.loop
    dey
    sta (W2),Y
    sta (W3),Y
    beq .done
    jmp .loop
.done
    rts
;;; vertical blit
;;; W1 = source bits
;;; W2 = left tile dest bits
;;; W3 = right tile dest bits
;;; S1 = amount to shift ( 1 - 8 )
;;; S2 = sprite to move
;;; uses W4,S5,S6
;;; 
blitd SUBROUTINE
#if _debug                      ;scroll amount should be in range of 0 - 7
        lda #8
        cmp S1
        bcs .0
        brk
.0
#endif
        ;; get left hand tile 'underneath' bitmap
        ;; so we can or it into this new image
        ldx S2                 
        lda Sprite_sback,X      ;input to mergeTile
        bne .00
        brk
.00        

        mergeTile               ;font address of underneath tile into W4

        ldx #0
        ldy #0
.loop        
        lda (W4),Y
        cpx S1
        bmi .lt
        ora (W1),Y
        inc16 W1
.lt                             ;don't copy from source
        sta (W2),Y
        inc16 W4
        inc16 W2
        inx                     ;next x
        cpx #8
        bmi .loop

        ldx S2
        lda Sprite_sback2,X
        bne .01
        brk
.01        
        mergeTile

        ldx #0                  

.loop2
        lda (W4),Y
        cpx S1
        bpl .lt1
        ora (W1),Y
        inc16 W1
.lt1                             ;don't copy from source
        sta (W3),Y
        inc16 W4
        inc16 W3
        inx                     ;next x
        cpx #8
        bmi .loop2

.done
        rts
#if _debug        
;;; X sprite background tiles to dump
dumpBack SUBROUTINE
        txa
        asl
        tay
        lda Sprite_back,X      ;input to mergeTile
        bne .ok0
        brk
.ok0        
        sta $1004,Y
        lda Sprite_back2,X
        bne .ok1
        brk
.ok1        
        sta $1005,Y
        rts
#endif        
;;; horizontal blit
;;; W1 = source bits
;;; W2 = left tile dest bits
;;; W3 = right tile dest bits
;;; W4 = under character bitmap
;;; S1 = amount to shift ( 1 - 8 )
;;; S2 = sprite to move
blith SUBROUTINE
#if _debug                      ;scroll amount should be in range of 0 - 7
    clc
    lda #8
    cmp S1
    bcs .0
    brk
.0
#endif
#if 1
        ldx S2
        txa
        asl
        pha
        tay
        lda Sprite_sback,X      ;input to mergeTile
        sta $1008,Y
        mergeTile               ;font address of underneath tile into W4
        move16 W4,W6

        pla
        tay
        lda Sprite_sback2,X
        sta $1009,Y
        mergeTile
#endif        
        ldy #7
        ldx S1
.nextbyte
        lda (W1),Y
        sta (W2),Y
        ldx S1
.loop
        beq .shiftdone
        lda (W2),y
        clc
        ror
        sta (W2),Y
        lda (W3),Y
        ror
        sta (W3),Y
        dex
        jmp .loop
.shiftdone
#if 1        
        lda (W6),Y              ;load left tile underneath bits
        ora (W2),Y              ;overlay to left tile
        sta (W2),Y
        lda (W4),Y              ;load right tile underneath bits
        ora (W3),Y              ;overlay to right tile
        sta (W3),Y
#endif        
        dey
        bmi .done
        jmp .nextbyte
.done
    rts
;;;
;;; X = sprite to move
scroll_up SUBROUTINE
        lda Sprite_dir,X
        cmp #22                     ;check if already vertical
        beq .00
        lda #1
        sta S1
        jsr changevert
        bcc .00
        rts
.00
        ldy Sprite_offset,X
        bne .fine               ; > 0 then fine scroll
.course                         ; course scroll
;        brk
        move16x Sprite_loc,W2   ; screen location to W2
        subxx W2,W1,#22
        jsr move_ok
        beq .done               ; move forbidden
        subxx W2,W1,#22         ;move up one tile
        move16x2 W1,Sprite_loc2  ;save the new sprite screen location
        ldy #8                  ;reset fine scroll offset
.fine
;        brk
        dey
        tya
        sta Sprite_offset2,X
.done
        clc
        rts

;;; change orientation to horizontal
;;; S1 direction to change to ( 22=left 24=right)
;;; S2 offset from W2 of erase tile
;;; -------------------------------------------------------
;; let L,H be left tile and right tile of pacman, let O be the origin
;;  
;;   #.#     #.#     #.#   ##O###  ######  ######
;; ##OL##  ###.##  ###.##  #..L..  #.LH..  #..LH.
;; ...H.   ..LH.   ...LH   ###H##  ###.##  ###.##
;; ######  ######  ######    #.#     #.#     #.# 
;;; 
changehoriz SUBROUTINE
        move16x Sprite_loc,W1
        lda #1                 ; offset to blank tile in endtile case
        sta S2
        sta S3                  ;-offset to ORG in endtile case
        lda Sprite_offset,X     ;where is sprite offset
        beq .begtile            ; at beginning of tile?
        cmp #8                  ; at end of tile?
        beq .endtile
.failed                         ;can't change direction yet
        sec
        rts
.begtile                        ;at beginning of tile
        lda #45                 ;change erase tile offset
        sta S2                  ;
        
        lda #23                 ;update offset to ORG
        sta S3                  ;for begtile case

.endtile                        ;end of tiles branch
        subxx W1,W2,S3          ;generate ORG address in W2
        
        ldy S1
        lda (W2),Y
        cmp #MW                 ;is it a wall
        beq .failed             ;we hit a wall, abort move
        
        lda #1                  ;change direction to horiz
        sta Sprite_dir2,X

        lda #0                  ;assume going right
        sta Sprite_offset,X     ;sprite offset = 0

        lda #23                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving right

        lda S1                  ;moving right?
        cmp #24
        beq .isright            ;branch moving right

        lda #8                  ;going left, sprite offset = 8
        sta Sprite_offset,X

        lda #22                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving up
.isright
        addxx W2,W1,S3          ;update new sprite pos
        move16x2 W1,Sprite_loc2

        clc                     ;return success
        rts


;;; change orientation of the 2 tiles that represent pacman
;;; to vertical
;;; C = true if failed
;;; X = sprite to move
;;; S1 direction of change ( 1=up 45=down)
;;; W2 = ORG position
;;; note, we update the spriteoffset instead of offset2 because
;;; we manipulate that quantity after this routine is called
;;; ------------------------------------------------------------------------
;;; up  NewP = PRG + 1     | up  NewP = PRG + 1
;;; dwn NewP = ORG + 23    | dwn NewP = Prg + 23
;;; O = org
;;; let L,H be left tile and right tile of pacman,
;;;   B4                                         
;;; ###O.#                                               
;;; ...LH#             O.#####                           
;;; ####.#             #LH..##                           
;;;                    #.#####  
;;; up       down                    or                
;;; ###OL#   ###O.#            O.###     OL####        
;;; ....H#   ....L#            #L..#     #H...#        
;;; ####.#   ####H#            #H###     #.####
;;; 
changevert SUBROUTINE
        move16x Sprite_loc,W1
        ldy #22                 ;offset of tile to erase in endtile case
        sty S2
        sty S3                  ;offset to ORG in endtile case
        lda Sprite_offset,X     ; where is sprite offset
        beq .begtile            ; at beginning of tile?
        cmp #8                  ; at end of tile?
        beq .endtile
.failed ; we can't change directions  in middle of tile
        sec
        rts
.begtile                        ;at beginning of tile
        ldy #24                 ;change erase tile offset
        sty S2                  ;
        dey                     ;ldy #23
        sty S3                  ;update ORG offset
.endtile                        ;end of tiles branch
        subxx W1,W2,S3          ;generate ORG address in W2
        
        ldy S1
        lda (W2),Y
        cmp #MW                 ;is it a wall
        beq .failed             ;we hit a wall, abort move
        
        lda #22                 ;change direction to down
        sta Sprite_dir2,X

        lda #0                  ;assume going down
        sta Sprite_offset,X     ;sprite offset = 0

        lda #23                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving down

        lda S1                  ;moving down?
        cmp #45
        beq .isdown             ;branch down

        lda #8                  ;going up, sprite offset = 8
        sta Sprite_offset,X

        lda #1                  ;offset from ORG for new sprite pos
        sta S3                  ;if moving up
.isdown
        addxx W2,W1,S3          ;update new sprite pos
        move16x2 W1,Sprite_loc2

        clc                     ;return success
        rts
;;; X = sprite to scroll down
scroll_down SUBROUTINE
        lda Sprite_dir,X
        cmp #22                 ;check if already vertical
        beq .00
        lda #45
        sta S1
        jsr changevert          ;if not, change us to down
        bcc .00
        rts
.00
        ldy Sprite_offset,X
        cpy #8                  ; less than 8? then fine scroll
        bmi .fine
.course                         ; course scroll
;        brk
        move16x Sprite_loc,W2   ; screen location to W2
        addxx W2,W1,#44
        jsr move_ok
        beq .done               ; move forbidden
        addxx W2,W1,#22
        move16x2 W1,Sprite_loc2  ;save the new sprite screen location
        ldy #0                  ;reset fine scroll offset
.fine
;        brk
        iny
        tya
        sta Sprite_offset2,X
.done
        clc
        rts
.dbg
        brk
;;; Y is clobbered
;;; S5 * S6 output ( 16 bit ) W5
multxx SUBROUTINE
;
; B * C [i.e] multiplier * multiplicand
; begin

        lda #0                  ; final result starts at 0
        sta W5
        sta W5+1
        ldy #8
loop:
        asl S5                  ; shift B to the right
        bcc nextdigit
        clc
        adc S6                  ; add C to the final result
        bcc nextdigit
        inc W5+1
nextdigit:
        dey                     ; loop counter dec
        beq done
        asl                     ; multiply by 2
rotatehighbyte:
        rol W5+1                ; dont need clc after this
        bcc loop
done:
        sta W5
        rts
        