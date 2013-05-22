        org $0400
        processor 6502
_LOCAL_SAVEDIR equ 1
;_SLOWPAC       equ 1            ;pacman doesn't have continuous motion
GHOSTS_ON   equ 1
_debug      equ 1                 ; true for debugging
cornerAdv   equ 1                 ;pacman's cornering advantage in pixels
voice1      equ 36874             ; sound registers
voice2      equ 36875
voice3      equ 36876
voice4      equ 36877        
volume      equ 36878
;screen     equ $1000        ; screen ram
screen      equ $1e00        
;clrram     equ $9400        ; color ram for screen
clrram      equ $9600             ; color ram for screen
clroffset   equ $78               ;offset from screen to color ram

VICRASTER equ $9004        
VICSCRN   equ $9005             ;vic chip character generator pointer
LIGHPENX  equ $9006             ;used for random number
VIA1DDR   equ $9113
VIA2DDR   equ $9122             ; ?
JOY0      equ $9111
JOY0B     equ $9120             ; output register B VIA #2
JOYUP     equ $4                ; joy up bit
JOYDWN    equ $8                ; joy down
JOYL      equ $10               ; joy left
JOYT      equ $20               ; joy fire
JOYR      equ $80               ; joy right bit

JIIFYH    equ $a0               ; jiffy clock lsb - msb
JIFFYM    equ $a1
JIFFYL    equ $a2

chrom1    equ $8000             ; upper case character rom
chrom2    equ $8400             ; upper case reversed
chrom3    equ $8c00             ; upper lower reversed
chrom4    equ $8800             ; upper lower

;; vic-I chip registers
chrst           equ $9003       ; font map pointer
;mychars         equ $1400        ; my font map
mychars         equ $1c00        
charcnt         equ $800
zeroDigit       equ 48 | $80    ;zero digit character
motionRight     equ 24          ;sprite locator code for right
motionLeft      equ 22          ;sprite locator code for left
motionUp        equ 1           ;sprite location code for up
motionDown      equ 45          ;" for down
tunnelRow       equ 11          ;row number that tunnel lives on
tunnelRCol      equ 20          ;column to start warp to left side
tunnelLCol      equ 1           ;column to start warp to right side
        
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

SPRITES      equ 5             ;count of sprites in system (1 based)
;;
;;  Zero page constants
;;
;;; W prefix vars are WORD width
;;; S or byte, short for 'scratch'
W1              equ 0
W2              equ 2
W3              equ 4        ; 16 bit work 3
W4              equ 6           ;2 byte work var
S0              equ 7        
S1              equ 8        ; 1 byte scratch reg
S2              equ 9        ; 1 byte scratch reg
S3              equ 10
S4              equ 11
        
#IFCONST _LOCAL_SAVEDIR
DIV22_WORK      equ $c          ;word
;;;                 13
DIV22_RSLT      equ $e          ;div22 result
#endif
;;; it's ok the next 2 use the same address
;;; they are never used at the same time
SPRITEIDX       equ $f        ;sprite index for main loop
MASTERCNT       equ $f        ;countdown; see masterDelay
;;; 
CSPRTFRM        equ $10        ; number of frames in the currently processing sprite
DOTCOUNT        equ $11        ;dots eaten
PACFRAMED       equ $12        ;pacframe dir
NXTSPRTSRC      equ $13        ;when moving a sprite, the next 'set' of source bitmaps
DSPL_1          equ $14        ;used by DisplayNum routine
BCD             equ $15        ;used by Bin2Hex routine
DSPL_2          equ $16        ;
DSPL_3          equ $17        ;
CHASEMODE       equ $18        ;ghosts in scatter mode or chase
GHOST_DIST      equ $19  ; $18 best distance for current ghost AI calcs
GHOST_DIR       equ $20  ; $19 best move matching GHOST_DIST
DIV22_REM       equ $21        
PACCOL          equ $22         ;current pacman column
PACROW          equ $23         ;current pacman row
;;; sprite position used by AI from most recent call to any of the
;;; directional changing routines ( up,left etc )
GHOST_TGTCOL    equ $24
GHOST_TGTROW    equ $25
GHOST1_TGTCOL   equ $26
GHOST1_TGTROW   equ $27
;;amount sprite move routines can shave off during cornering
;;; pacman get +1 on corners, ghosts get 0
CORNER_SHAVE    equ $28
POWER_UP        equ $29         ;pacman is powered up
        ;; 47 48 are toast?
;;; e.g. if pacman successfully moves up, then switch to PAC_UP1 set of source 
S5              equ $30
S6              equ $31
PACXPIXEL       equ $32
PACYPIXEL       equ $33        
#IFCONST _LOCAL_SAVEDIR
GHOST_COL       equ $35        
GHOST_ROW       equ $36
#ENDIF
SCORE_l         equ $37
SCORE_h         equ $38        
S7              equ $43
W5              equ $32
W6              equ $44
        ;; $45 has fucked me, not sure why, memmap lists it
        ;; as current variable name, sure sounds like a basic
        ;; interpreter thing to me 
        ;; 

;;; sentinal character, used in tile background routine
;;; to indicate tile background hasn't been copied into _sback yet
NOTCOPY         equ $fd
;;; used by the AI engine to indicate the worst possible choice
noChoice        equ $7f
        
;;;offset for normalizing a 9x9 sprite movement block to the upper left block
;;; used by the sprite orientation changing routines
SPRT_LOCATOR    equ $47
;value that indicates end of smooth scrolling
END_SCRL_VAL    equ $48
;;;amount to increase or decrease sprite offset
;;; used as input by scroll_horiz
SCRL_VAL        equ $49
LASTJOY         equ $4a
LASTJOYDIR      equ $4b         ;last joy reading that had a switch thrown
MOVEMADE        equ $4c         ;true if last pacman move was successful
TIMER1          equ $4d         ;decrement by main game loop every other trip
TIMER1_h        equ $4e         ;timer1 high byte
r_seed          equ $4f        
#if 0
;;; just for testing how many bytes I could save
;;; if I moved these to zero page -- about 120 bytes right now
Sprite_loc      equ $4d
Sprite_loc2     equ $4f        
Sprite_back      equ $4d
Sprite_back2      equ $4d
Sprite_sback      equ $4d
Sprite_sback2      equ $4d
Sprite_tile      equ $4d
Sprite_src      equ $4d
Sprite_frame      equ $4d
Sprite_bmap      equ $4d
Sprite_bmap2      equ $4d
Sprite_motion      equ $4d
Sprite_turn      equ $4d
Sprite_color      equ $4d
Sprite_mode      equ $4d
Sprite_speed      equ $4d
Sprite_offset      equ $4d
Sprite_offset2     equ $4d
Sprite_dir      equ $4d
Sprite_dir2     equ $4d
#endif        
        
#IFCONST _LOCAL_SAVEDIR
SAVE_OFFSET     equ $ab
SAVE_OFFSET2    equ $ac
SAVE_DIR        equ $ad
SAVE_DIR2       equ $ae
#endif
        
VV              equ $02         ;testing, voice 2
;;
;; misc constants
;;
PACL            equ $00            ; pacman char number

GHL             equ $0b            ; ghost char number
GH1L            equ [GHL+4]
GH2L            equ [GH1L+4]
GH3L            equ [GH2L+4]    
DOT             equ $04
WALLCH          equ $05
MW              equ $05            ;maze wall character
PWR             equ $06
HWALL           equ $07
EYES            equ $08        
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
        ;; logical not of 1, used to switch between on/off states
        MAC Invert
        lda #1                  ;dbl buffering, switch sprite tiles
        eor {1}         ; 0 = 1
        sta {1}         ; or 1 = 0
        ENDM
        ;; save X
        MAC saveX
        txa
        pha
        ENDM
        ;; restore X
        MAC resX
        pla
        tax
        ENDM
        ;; compare {1} with #{2} 
        MAC cmp16Im
        lda {1}
        cmp #[{2}] & $ff    ; load low byte
        bne .done
        lda {1}+1
        cmp #{2} >> 8     ; load high byte
.done        
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
        ;; double a signed byte
        MAC DoubleSigned
        bpl .positive
        Abs                     ;absolute value
        asl                     ;times 2
        MakeNegative            ;make negative again
        bmi .done
.positive
        asl                     ;simple times 2
.done        
        ENDM
        ;; compare word in {1} with {2}
        MAC cmp16
        lda {1}+1
        cmp {2}+1
        bne .done
        lda {1}
        cmp {2}
.done        
        ENDM
;;; initialize the sprite loop counter
        MAC InitSpriteLoop
        lda #SPRITES
        sta SPRITEIDX
        ENDM
;;; display a single byte {3} at offset {2} on top line prefixed by char {1}
        MAC Display1
        pha                     ;save A
        txa
        pha
        
        lda #[{1}-"A"+1 | $80]
        ldx #{2}
        sta screen,X
        lda clrram,X
        and #%00000111          ;read existing color
        clc
        adc #1                  ;add one to it
        cmp #8                  ;wrap around
        bne .storeit
        lda #WHITE
.storeit        
        sta clrram,X
        inx
#if 0                           ;decimal
        lda {3}
        jsr DisplayNum
#else                           ;hex
        lda {3}
        sta BCD
        jsr DisplayBCD
#endif        
        pla
        tax

        pla                     ;restore A

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
    ;; ABORT instruction
    mac abort
    lda #$c0
    sta VICSCRN
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
        ;; substract byte from word
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

        MAC sub16Im
        sec
        lda {1}
        sbc #[{2}]&$ff
        sta {1}
        lda {1}+1
        sbc #[{2}]>>8
        sta {1}+1
        ENDM
        ;; ghosts to chase mode
        ;; important: Z false on exit
        MAC ChaseMode

        Display1 "C",10,#1
        store16 255,TIMER1
        lda #1
        sta CHASEMODE
        
        ENDM
        ;; ghosts to Scatter mode
        ;; Z true on exit
        MAC ScatterMode

        store16 110,TIMER1

        Display1 "S",10,TIMER1
        lda #0
        sta CHASEMODE
        
        ENDM
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
;; beq on joy right A has bit 7 of last reading from JOY0B
        MAC onjoyr
        lda #127
        sta VIA2DDR             ;setup VIA for joystick read
        lda JOY0B               
        pha
        lda #$ff                ;restore VIA2 so keyboard can be read
        sta VIA2DDR
        pla                     ;pull out  our joystick reading
        and #JOYR
        ENDM
        
        MAC readJoy
        
        lda JOY0
        and #$7f                ;clear bit 7 ( joy right )
        sta LASTJOY
        onjoyr                  ;read joystick right pos
        ora LASTJOY             ;or in bit 7 from that read
        sta LASTJOY             ;store it
.done        
        ENDM
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

MazeB

    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00000001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010000
    dc.b %01010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010001
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01000100
    dc.b %00010110
    dc.b %01001010
    dc.b %00100100
    dc.b %10010100
    dc.b %01010001
    dc.b %00100100
    dc.b %10100010
    dc.b %01011001
    dc.b %00000101
    dc.b %00010010
    dc.b %10001001
    dc.b %00100101
    dc.b %00010100
    dc.b %01001001
    dc.b %00101000
    dc.b %10010100
    dc.b %01000001
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00010000
    dc.b %01010001
    dc.b %00101000
    dc.b %10100010
    dc.b %01001001
    dc.b %00100100
    dc.b %10100010
    dc.b %10001001
    dc.b %01000100
    dc.b %00010100
    dc.b %10010010
    dc.b %00101001
    dc.b %00100100
    dc.b %01010010
    dc.b %01001000
    dc.b %10100100
    dc.b %10010001
    dc.b %00000100
    dc.b %10010010
    dc.b %10001001
    dc.b %00100101
    dc.b %00010100
    dc.b %01001001
    dc.b %00101000
    dc.b %10010010
    dc.b %01000000
    dc.b %00000000
    dc.b %10100010
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00001010
    dc.b %00100000
    dc.b %00000000
    dc.b %01001001
    dc.b %00101000
    dc.b %10000010
    dc.b %01001000
    dc.b %00100100
    dc.b %10000010
    dc.b %10001001
    dc.b %00100100
    dc.b %00000000
    dc.b %00000010
    dc.b %00000000
    dc.b %10000000
    dc.b %00000000
    dc.b %00100000
    dc.b %00100000
    dc.b %00000000
    dc.b %00000100
    dc.b %10010010
    dc.b %10001000
    dc.b %00100100
    dc.b %10010010
    dc.b %01001000
    dc.b %00101000
    dc.b %10010010
    dc.b %01000000
    dc.b %00000000
    dc.b %10100010
    dc.b %00000000
    dc.b %00000000
    dc.b %00000000
    dc.b %00001010
    dc.b %00100000
    dc.b %00000000
    dc.b %01001001
    dc.b %00101000
    dc.b %10100010
    dc.b %01001001
    dc.b %00100100
    dc.b %10100010
    dc.b %10001001
    dc.b %00100100
    dc.b %00010100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %01010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010001
    dc.b %00000101
    dc.b %00010010
    dc.b %10001001
    dc.b %00100101
    dc.b %00010100
    dc.b %01001001
    dc.b %00101000
    dc.b %10010100
    dc.b %01000001
    dc.b %01001000
    dc.b %10100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %00101001
    dc.b %00010000
    dc.b %01001010
    dc.b %00101000
    dc.b %10100010
    dc.b %01001001
    dc.b %00100100
    dc.b %10100010
    dc.b %10001010
    dc.b %00100100
    dc.b %00010110
    dc.b %10010010
    dc.b %00101001
    dc.b %00100100
    dc.b %01010010
    dc.b %01001000
    dc.b %10100100
    dc.b %10011001
    dc.b %00000101
    dc.b %00010010
    dc.b %01001001
    dc.b %00100101
    dc.b %00010100
    dc.b %01001001
    dc.b %00100100
    dc.b %10010100
    dc.b %01000001
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00010000
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
    dc.b %10010010
    dc.b %01001001
    dc.b %00100100
MazeX


pacframes  equ #4            ; total number of pacman animation frames ( 1 based )
;;; 
;;; define some 8x8 characters
;;; 
#if 1

PAC1
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC2
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11110000
    ds 1,%11110000
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC3
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111000
    ds 1,%11110000
    ds 1,%11110000
    ds 1,%11111000
    ds 1,%01111110
    ds 1,%00111100
PAC4
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11110000
    ds 1,%11100000
    ds 1,%11100000
    ds 1,%11110000
    ds 1,%01111110
    ds 1,%00111100
;;; --------------
PAC1D
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC2D
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11100111
    ds 1,%11100111
    ds 1,%01100110
    ds 1,%00100100
PAC3D
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11100111
    ds 1,%11000011
    ds 1,%01000010
    ds 1,%00000000
PAC4D
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11100111
    ds 1,%11000011
    ds 1,%11000011
    ds 1,%01000010
    ds 1,%00000000
;;; -------------
PAC_UP1
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC_UP2
    ds 1,%00100100
    ds 1,%01100110
    ds 1,%11100111
    ds 1,%11100111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC_UP3
    ds 1,%00000000
    ds 1,%01000010
    ds 1,%11000011
    ds 1,%11100111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC_UP4
    ds 1,%00000000
    ds 1,%01000010
    ds 1,%11000011
    ds 1,%11000011
    ds 1,%11100111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
;;; --------------------------
PAC_L1
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC_L2
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%11111111
    ds 1,%00001111
    ds 1,%00001111
    ds 1,%11111111
    ds 1,%01111110
    ds 1,%00111100
PAC_L3
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%00011111
    ds 1,%00001111
    ds 1,%00001111
    ds 1,%00011111
    ds 1,%01111110
    ds 1,%00111100
PAC_L4
    ds 1,%00111100
    ds 1,%01111110
    ds 1,%00001111
    ds 1,%00000111
    ds 1,%00000111
    ds 1,%00001111
    ds 1,%01111110
    ds 1,%00111100
        
GHOST
    ds 1,%01111110
    ds 1,%11000011
    ds 1,%11010111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11100011
    ds 1,%11111111
    ds 1,%10101010
GHOST2        
    ds 1,%01111110
    ds 1,%11000011
    ds 1,%11010111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11100011
    ds 1,%11111111
    ds 1,%01010101
    ;; ds 1,215
    ;; ds 1,255
    ;; ds 1,255
    ;; ds 1,227
    ;; ds 1,255
    ;; ds 1,170
#else

GHOST
    ds 1,1
    ds 1,2
    ds 1,4
    ds 1,8
    ds 1,16
    ds 1,32
    ds 1,64
    ds 1,128

PAC1 ds 8,$01
PAC2 ds 8,$01
PAC3 ds 8,$01
PAC4 ds 8,$01
#endif

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
;;;
;;; there are 4 sets of frames
;;; for each sprite
;;; the order of sets is left,right,top,bottom
;;; 
;;; debugging notes: 22*5+5 is at an intersection
;;; for testing up/down/left/right transitions
;------------------------------------
dirVert         equ 22            ;sprite oriented vertically
dirHoriz        equ 1             ;sprite oriented horizontally
modeOutOfBox    equ 1             ;see sprite_mode
modeLeaving     equ 2             ;leavin the ghost box
modeEaten       equ 3             ;ghost was chomped
outOfBoxCol     equ 11            ;column at ghost box entry/exit
outOfBoxRow     equ 9             ;row of tile above exit
pinkyHomeCol    equ 3
pinkyHomeRow    equ 3
blinkyHomeCol   equ 22-3
blinkyHomeRow   equ 3
clydeHomeCol    equ 6
clydeHomeRow    equ 23-2
inkyHomeCol     equ 23-6
inkyHomeRow     equ 23-2        
;;; screen location of ghost box exit
ghostBoxExit    cmp [screen+[22*outOfBoxRow]+outOfBoxCol]
;;; the ghost's by X register offset
inky            equ 1        
blinky          equ 2
pinky           equ 3
clyde           equ 4
focusGhost      equ 10          ;ghost to print debugging for
fruit1Dots      equ 70          ;dots to release fruit
fruit2Dots      equ 120         ;dots to release fruit2
clydeDots       equ 255          ;dots to release clyde ( about 33% )
inkyDots        equ 255          ;dots to release inky  ( )
pinkyDots       equ 255           ;dots to release pinky ( should be 1)
;;; 
Sprite_page     dc.b 0        
#if 1
Sprite_loc      DC.W 0,0,0,0,0    ;screen loc
Sprite_loc2     DC.W screen+22*2+8 ,screen+22*11+9,screen+22*(9+12)+11,screen+22*11+10,screen+22*11+12    ;new screen loc
Sprite_back     dc.b 0,0,0,0,0           ;background char value before other sprites are drawn
Sprite_back2    dc.b 0,0,0,0,0           ;static screen background
Sprite_sback    dc.b 0,0,0,0,0 ;current screen background ( might include some other sprite tile that was laid down )
Sprite_sback2   dc.b 0,0,0,0,0        
Sprite_tile     dc.b PACL,GHL,GH1L,GH2L,GH3L      ;foreground char
Sprite_src      dc.w PAC1,GHOST,GHOST,GHOST,GHOST ;sprite source bitmap
Sprite_frame    dc.b 0,1,1,1,1 ;animation frame of sprite as offset from _src
;;; sprite chargen ram ( where to put the source bmap )
Sprite_bmap     dc.w mychars+(PACL*8),      mychars+(GHL*8)      ,mychars+(GH1L*8)     , mychars+(GH2L*8)     , mychars+(GH3L*8)    
Sprite_bmap2    dc.w mychars+(PACL*8)+(2*8),mychars+(GHL*8)+(16) ,mychars+(GH1L*8)+(16), mychars+(GH2L*8)+(16),mychars+(GH3L*8)+(16)
Sprite_motion   dc.b 1,motionRight,motionLeft,motionRight,motionLeft ; see motion defines
Sprite_dir      dc.b 1,1,1,1,1 ;sprite direction 1(horiz),22(vert)
Sprite_dir2     dc.b 1,1,1,1,1 ;sprite direction 1(horiz),22(vert)    
Sprite_offset   dc.b 0,0,0,2,6  ;sprite bit offset in tiles
Sprite_offset2  dc.b 0,0,0,2,6  ;sprite bit offset in tiles
Sprite_speed    dc.b 80,20,10,20,20 ;your turn gets skipped every N loops of this
Sprite_turn     dc.b 5,4,4,4,4        
Sprite_color    dc.b #YELLOW,#CYAN,#RED,#GREEN,#PURPLE
        ;; in order of exit 
        ;; red=blinky ( starts outside of ghost house )
        ;; green = pinky AI
        ;; cyan =inky 
        ;; purple = clyde
        
        ;; if eyes heading toward ghost box
        ;; or in ghost box already
        ;; etc
Sprite_mode    dc.b 1,0,1,0,0  ;in ghost box if false
#endif        
masterSpeed      equ 15           ;master game delay
#IFNCONST
SAVE_OFFSET     dc.b 0
SAVE_OFFSET2    dc.b 0
SAVE_DIR        dc.b 0
SAVE_DIR2       dc.b 0
GHOST_COL       dc.b 0
GHOST_ROW       dc.b 0
DIV22_RSLT      dc.b 0
DIV22_WORK      dc.w 0
#ENDIF        
;;; 
;;; division table for division by 22
Div22Table      dc.w [22*16],[22*8],[22*4],[22*2],[22*1]
GhosthomeTable  dc.b inkyHomeCol,inkyHomeRow,blinkyHomeCol,blinkyHomeRow,pinkyHomeCol,pinkyHomeRow,clydeHomeCol,clydeHomeRow
MotionTable     dc.b motionUp,motionDown,motionLeft,motionRight        
;;; swap upcoming sprite data with current sprite data
;;; i.e. page flip the screen location
        MAC SwapSpritePos
        
        move16xx Sprite_loc2,Sprite_loc
        lda Sprite_dir2,X
        sta Sprite_dir,X
        lda Sprite_offset2,X
        sta Sprite_offset,X

        ENDM
;;; S2 sprite to render
render_sprite SUBROUTINE
        stx S2                  ;set for call to blith
#if _debug        
        lda #SPRITES
        cmp S2
        bcs .ok
        brk
.ok
#endif
;        jsr dumpBack2
        move16x Sprite_src,W1   ;bitmap source -> W1
        lda Sprite_frame,X
        asl                     ;mul by 8
        asl
        asl
        clc
        adc W1
        sta W1
        lda #0
        adc W1+1
        sta W1+1
        
        ;; assume tile pair 0
        ;; if we are currently on tile pair 0 , then render into pair 1
        move16x Sprite_bmap2,W2  ;left tile chargen ram
        ldy #0
        cpy Sprite_page
;        ldy #1
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
        cpx #0
        bne .notpac
        lda #4
        cmp Sprite_offset
        bne .notpac
        lda #$20
        sta Sprite_back
        sta Sprite_back2
.notpac        
        move16x Sprite_loc,W1   ;sprite location to W1
        ldy #0
        lda Sprite_back,X
        sta (W1),Y
        
        ldy Sprite_dir,X
        lda Sprite_back2,X

        sta (W1),Y              ;restore tail tile to playfied
        ldy #WHITE
        jsr UpdateColorRam
        rts
        ;; 
        ;; load the upcoming ( to be rendered ) tile
        ;;  into A
        ;; X = sprite in question
        ;; Y corrupted
        mac loadTile
        lda Sprite_tile,X
        ldy Sprite_page
        beq .page0
        clc
        adc #2
.page0
        endm
        mac loadTile2
        lda Sprite_tile,X
        ldy Sprite_page
        bne .page0
        clc
        adc #2
.page0
        endm
        ;; determine ghost drawing color
        MAC GetSpriteColorInY
        ldy Sprite_color,X
        cpx #0                  ;pacman doesn't change colors
        beq .done
        lda POWER_UP
        beq .done
        ;; flash ghost color when power up is low
        cmp #20
        bcc .near_end
        ldy #BLUE
        jmp .done
.near_end                       ;almost out of power time
        ldy #WHITE
        lda #4
        bit POWER_UP
        beq .done
        ldy #BLUE
.done
        ENDM
;;; Y =  byte of color
;;; W1 = screen position of sprite
;;; X = sprite we are working with
UpdateColorRam SUBROUTINE
        lda #clroffset          ;W1 now = color ram location
        clc
        adc W1+1
        sta W1+1
        tya         ;color to A
        ;; below checks if we need to set the color in the
        ;; head, tail or both head and tail 
        ldy Sprite_offset,X     ;check if we need to update
        cpy #8                  ;color in head
        beq .intailonly         ;sprite lives in tail only
        ldy #0
        sta (W1),Y              ;write head tile color ram
.intailonly        
        ldy Sprite_offset,X     ;check if we need to update
        beq .singletile
        ldy Sprite_dir,X        ;write tail tile color ram 
        sta (W1),Y
.singletile        
        rts
        rts
;;; TODO: the screen backing tiles needs to check to 'upcoming' sprite tile positions
;;; X = sprite to move
;;; uses W2,S2
drwsprt1 SUBROUTINE
        move16x Sprite_loc,W1

        ;; figure out which set of tiles we should be rendering
        ;; we 'page flip' between tiles for speed
        loadTile                ;upcoming tile into A
        
        ldy #0   
        sta (W1),Y              ;put head tile on screen
        clc
        adc #01                 ;inc to tail tile

;        jmp .skip
        ldy Sprite_dir,X        ;are we vertical or horizontal?
.skip        
        sta (W1),Y              ;lay down the tail tile

        GetSpriteColorInY

        jsr UpdateColorRam
        rts

 INCLUDE "audio.asm"

        ;;
        ;;close the ghost box door
        ;; A=box cover character on exit
        MAC CloseGhostBox
        lda #BLUE
        sta clrram+22*[outOfBoxRow+1]+outOfBoxCol
        lda #MW
        sta screen+22*[outOfBoxRow+1]+outOfBoxCol
        ENDM
ReverseGhost SUBROUTINE
        InitSpriteLoop          ;foreach sprite
.loop        
        dec SPRITEIDX
        beq .done
        ldx SPRITEIDX
        lda Sprite_motion,X
        cmp #motionRight
        beq .right
        cmp #motionDown
        beq .down
        cmp #motionLeft
        beq .left
        cmp #motionUp
        beq .up
        brk
.left
        lda #motionRight
        sta Sprite_motion,X
        bne .loop
.right
        lda #motionLeft
        sta Sprite_motion,X
        bne .loop
.up
        lda #motionDown
        sta Sprite_motion,X
        bne .loop
.down
        lda #motionUp
        sta Sprite_motion,X
        bne .loop
.done
        rts
Timer1Expired SUBROUTINE
;        jsr ReverseGhost
        lda CHASEMODE
        beq .chase

        ScatterMode

        beq .done
.chase        
        ChaseMode
.done
        rts
        
PowerPill SUBROUTINE
        lda #100
        sta POWER_UP
;        lda #11
;        sta 36879
        rts
;-------------------------------------------
; MAIN()
;-------------------------------------------
main SUBROUTINE
#if 0
        lda #$ea
        DoubleSigned
        brk
#endif
#if 0
        lda #$ea
        STA BCD
        ldx #0
        jsr DisplayBCD
        brk
#endif
        lda #1
        sta PACFRAMED

        lda #$ff
        sta LASTJOYDIR
        ;; huh, not surprisingly, if you don't run the cli
        ;; the keyboard doesn't work
        ;; but who turned it off? weird
        cli                     ; enable interrupts for jiffy clock
        lda JIFFYL
        sta r_seed
        lda #8
        sta 36879               ; border and screen colors
        sta volume              ; turn up the volume to 8

        lda #0
        sta $9113               ;joy VIA to input
        sta DOTCOUNT            ;dot count to 0
        sta POWER_UP            ;power up to 0

        jsr copychar            ; copy our custom char set

        jsr mkmaze

        CloseGhostBox
        ScatterMode         ;start ghosts out in scatter mode

        ldx #22
        lda #WHITE
.top
        sta clrram,X
        dex
        bne .top
.done

;        LoadTrack 1,TrackBass
;        LoadTrack 2,TrackHigh
        LoadTrack 3,Track1

        jmp .background
.loop
        lda #masterSpeed
        sta MASTERCNT
.iloop        
        lda VICRASTER           ;load raster line
        bne .iloop


;        ldx #1                     ; service voice VV
;        jsr VoiceTrack_svc         ; run sound engine
        
        dec MASTERCNT
        bne .iloop
        ;; ok, we are at vertical blank, on one of the frames we want to render
        ;; here we go ...
        ldx #3
        jsr VoiceTrack_svc          ; run sound engine

        Invert Sprite_page      ;dbl buffering, switch sprite tiles

        ;; decrement game based timers such as power pills and attack/scatter
        ldy POWER_UP
        beq .skip
        dey
        sty POWER_UP
.skip
        cmp16Im TIMER1,#0
        beq .start
        DEC16 TIMER1
        bne .start
        ;; notify timer1 expired
        jsr Timer1Expired
.start        
;        Display1 "T",0,TIMER1
        Display1 "J",0,JIFFYL
        InitSpriteLoop          ;foreach sprite
.eraseloop
        dec SPRITEIDX
        bmi .background
        ldx SPRITEIDX
        jsr erasesprt
        jmp .eraseloop
        ;; loop to collect the playfield background tiles
.background
        InitSpriteLoop          ;foreach sprite
.backloop        
        dec SPRITEIDX
        bmi .draw
        ldx SPRITEIDX
        ;; swap the upcoming position to the current    
        SwapSpritePos    
        ;; collect playfied background tiles so we can replace them later
        move16x Sprite_loc,W1
        ldy #0
        lda (W1),Y
        sta Sprite_back,X
        cpx #0
        bne .notpac
        cmp #PWR
        bne .0
        jsr PowerPill
.0
        cmp #DOT
        beq .notpac
        saveX
        ldx #3

;        HaltTrack
        resX
.notpac
        ldy Sprite_dir,X
        lda (W1),Y
        sta Sprite_back2,X
        cpx #0
        bne .backloop
        cmp #PWR
        bne .backloop
        jsr PowerPill
        ;; end collection of background tiles
        jmp .backloop
        
.draw
        InitSpriteLoop          ;foreach sprite
.drawloop
        dec SPRITEIDX                
        bmi .player
        ldx SPRITEIDX
#if _debug        
;        jsr dumpBack
#endif        
        jsr drwsprt1             ;draw in new location
        jmp .drawloop
.player
;;; this is the beginning of non-time critical stuff
;;; everything before this, we hoped had been completed on the vertical blank
;        jsr WaitFire
        jsr Pacman
#IFCONST GHOSTS_ON        
        jsr GhostAI
#endif
;        jsr PixelPos

        InitSpriteLoop
.playerloop
        dec SPRITEIDX
        bmi .loopend
        ldx SPRITEIDX
        jsr blargo
        ldx SPRITEIDX
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
    store16 chrom1+[8*GHL],W2
    store16 mychars+[8*GHL],W3
    store16 charcnt-[8*GHL],W1

    jsr movedown

    lda VICSCRN
    and #$f0
    ora #$0f                    ;char ram pointer is lower 4 bits
    sta VICSCRN

    rts
;;; 
;;; Create the maze
;;; 
mkmaze SUBROUTINE
        
        store16 MazeB-1,W1
        store16 screen,W2
        store16 clrram,W3

        ldx #0                  ;3 bit counter to 0
        stx S1                  ;3 bit accumulator start at 0

.fetch_byte
        inc16 W1                ;move pointer forward
        cmp16Im W1,MazeX
        bne .begin
        rts
.begin        
        ldy #0                  ;8 bit counter to 0
        lda (W1),Y              ;load compressed byte
        sta S2                  ;save working byte

.loop
        asl S2                  ;strip off leading bit
        rol S1                  ;rotate in the new bit
        inx                     ;increment 3 bit counter
        cpx #3                  ;have we read all three bits?
        bne .continue           ;no, keep reading
        jsr process_code0       ;yes, process the compressed code
.continue                       
        iny                     ;increment the 8 bit counter
        cpy #8                  ;have we process 8 bits?
        beq .fetch_byte         ;yes, fetch the next byte
        bne .loop               ;no, read another bit

process_code0 SUBROUTINE
        tya                     ;save Y
        pha
        ldy #0
        jsr process_code        ;dcode the char
        sta (W2),Y              ;store decoded char to screen
        txa
        sta (W3),Y
        sty S1                  ;reset 3  bit accumulator to 0
        ldx #0                  ;reset 3 bit counter
        pla                     ;restore Y
        tay
        inc16 W2                ;increment screen pos pointer
        inc16 W3
        rts
        
process_code SUBROUTINE
        lda S1                  ;pull 3 bit code
        beq .space
        cmp #%010
        beq .dot
        cmp #%001
        beq .wall
        cmp #%100
        beq .hwall              ;horizontal wall section
        cmp #%011
        beq .pdot
        lda #$20                ;default to space
        rts
.pdot
        lda #PWR
        ldx #WHITE
        rts
.hwall
        lda #HWALL
        ldx #BLUE
        rts
.wall
        lda #MW                 ;wall character
        ldx #BLUE
        rts
.dot
        lda #DOT
        ldx #WHITE
        rts
.space
        lda #$20
        ldx #BLACK
        rts
; Move memory down
;
; W2 = source start address
;   W3 = destination start address
; SIZE = number of bytes to move
;
movedown SUBROUTINE
        LDY #0
        LDX W1+1
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
        LDX W1
        BEQ md4
md3
        LDA ( W2 ),Y ; move the remaining bytes
        STA ( W3 ),Y
        INY
        DEX
        BNE md3
md4
        RTS


;;; waits for joystick to be pressed
;;; and released
WaitFire SUBROUTINE
.loop        
        lda JOY0                ; read joy register
        tay
        and #JOYT               ;was trigger pressed?
        beq .loop1
        tya
         ;; and #JOYDWN
         ;; beq .lbrk
        jmp .loop
.lbrk
        brk
.loop1                          ;wait for trigger to be released
        lda JOY0
        and #JOYT
        bne .fire
        jmp .loop1
.fire
        rts

        MAC scroll_left

        lda #22
        sta SPRT_LOCATOR
        lda #0
        sta END_SCRL_VAL
        lda #$ff                ;-1 into A
        sta SCRL_VAL
        jsr scroll_horiz
        ENDM

        MAC scroll_right
        
        lda #24
        sta SPRT_LOCATOR
        lda #8
        sta END_SCRL_VAL
        lda #$01
        sta SCRL_VAL
        jsr scroll_horiz
        ENDM
#if 1
;;; move ghost in its currently indicated direction
MoveGhost SUBROUTINE
        lda Sprite_mode,X       ;ghost in box don't get to move
        beq .exit
        lda Sprite_motion,X
        cmp #motionRight
        beq .right
        cmp #motionDown
        beq .down
        cmp #motionLeft
        beq .left
        cmp #motionUp
        beq .up
        brk
.left
        scroll_left
.exit        
        rts
.right
        scroll_right
        rts
.up
        jsr scroll_up
        rts
.down
        jsr scroll_down
        rts
#endif
;;; move sprite side to side
        MAC moveS
        lda Sprite_motion,X
        cmp #01
        beq .right
        scroll_left
        bcs .reverse
        bcc .done
.right
        scroll_right
        bcs .reverse
        bcc .done
.reverse
        lda Sprite_motion,X
        bmi .add
        lda #$fd
.add
        clc
        adc #$02
;        brk
        sta Sprite_motion,X
.done        
        ENDM
        
;;; move sprite up and down
        MAC moveD
        lda Sprite_motion,X
        cmp #22
        beq .down
        jsr scroll_up
        bcs .reverse
        bcc .done
.down
        jsr scroll_down
        bcs .reverse
        bcc .done
.reverse
        lda Sprite_motion,X
        cmp #22
        beq .0
        bne .1
.0
        lda #$EA                 ;-22
        jmp .store
.1
        lda #22
.store        
        sta Sprite_motion,X
.done
        ENDM
#if 1    
;;; move a ghost using the keyboard
GhostAsPlayer SUBROUTINE
        lda 197
        cmp #26
        beq .down
        cmp #9
        beq .up
        cmp #17
        beq .left
        cmp #18
        beq .right
        rts
.left
        scroll_left
        rts
.right
        scroll_right
        rts
.up
        jsr scroll_up
        rts
.down
        jsr scroll_down         ;
        rts
#endif
SpecialKeys SUBROUTINE
        lda 197
        cmp #9                  ;'w'
        bne .done

        saveX

        ldx #inky
        jsr LeaveBox
        resX

.done        
        rts

        
UpdateMotion SUBROUTINE
        lda GHOST_DIR
        sta Sprite_motion,X
        rts
#if 1        
;;; update motion but don't reverse
UpdateMotion2 SUBROUTINE        
        lda GHOST_DIR
        cmp #motionRight
        beq .right
        cmp #motionDown
        beq .down
        cmp #motionLeft
        beq .left
        cmp #motionUp
        beq .up
        cmp #$7f
        beq .done
        brk
.left
        lda #motionRight
        cmp Sprite_motion,X
        bne .store
;        brk
        rts
.right
        lda #motionLeft
        cmp Sprite_motion,X
        bne .store
;        brk
        rts
.up
        lda #motionDown
        cmp Sprite_motion,X
        bne .store
;        brk
        rts
.down
        lda #motionUp
        cmp Sprite_motion,X
        bne .store
;        brk
        rts
.store
        lda GHOST_DIR
        sta Sprite_motion,X
.done        
        rts
#endif        
        ;; calculate the ghost in X's
        ;; currently displayed row and column
        ;; Output: GHOST_ROW,GHOST_COL
        MAC CalcGhostRowCol
        
        move16x Sprite_loc,W1
        sub16Im W1,screen
        jsr Divide22_16
        lda W1
        sta GHOST_COL
        lda DIV22_RSLT
        sta GHOST_ROW
        
        ENDM
        ;; pac upcoming row col into variables
        ;; OUTPUT: PACCOL,PACROW
        MAC CalcPacRowCol
        
        move16 Sprite_loc2,W1
        sub16Im W1,screen
        jsr Divide22_16
        lda W1
        sta PACCOL              ;store tile column
        asl                     ;multiply by 8
        asl
        asl
        ldy Sprite_dir2         ;are we oriented vertically?
        cpy #dirVert
        beq .vert
        clc                     ;we are horiz
        adc Sprite_offset2      ;add in the smooth scroll offset to pixel count
.vert
        sta PACXPIXEL           ;store pixel column
        
        lda DIV22_RSLT          ;get row result from division
        sta PACROW              ;store tile row
        asl                     ;multiply by 8
        asl
        asl
        cpy #dirHoriz
        beq .horiz         ;
        clc                ;we are vertical
        adc Sprite_offset2 ; add in the smooth scroll offset to pixel count
.horiz        
        sta PACYPIXEL           ;store pixel row
        ENDM
        
        ;; check if sprite X get's to move this frame
        MAC MyTurn2
        dec Sprite_turn,X
        bne {1}
        ;; we don't get to move this turn
        ;; reset the turn counter, and set the carry
        ;; to indicate no soup for you
        lda Sprite_speed,X
        sta Sprite_turn,X
        sec
        ENDM
        ;; 
        ;; execute a queued move
        ;; 
        MAC ExeuteQueuedMove
        lda Sprite_motion,X
        cmp #motionRight
        beq .go_up
        cmp #motionLeft
        beq .go_up
        ;; go left
        lda #motionLeft
        sta Sprite_motion,X
        bne .done
.go_up
        lda #motionUp
        sta Sprite_motion,X
.done        
        ENDM
;;; animate a ghost back and forth
GhostAI SUBROUTINE
        lda #0
        sta CORNER_SHAVE        ;ghosts get no cornering bonus
        ;; caculate pacman row,col so ghosts can use
        ;; in their AI routines

        CalcPacRowCol

        InitSpriteLoop
.loop
        dec SPRITEIDX
        bne .notpac
        rts                     ;pacman is sprite 0, so we leave
.notpac        

        ldx SPRITEIDX
        MyTurn2 GhostTurn      ;does this ghost get to move this time?
        jmp .loop              ;no he doesn't
GhostTurn
        lda Sprite_mode,X
        cmp #modeLeaving
        beq .leaving
        cmp #modeEaten
        beq .eaten
        jmp .normal
.eaten                          ;load target tile for eaten ghosts
        lda #outOfBoxCol
        sta GHOST_TGTCOL
        lda #outOfBoxRow+2
        sta GHOST_TGTROW
        jmp .continue
.leaving                        ;load target tile for ghosts leaving box
        lda #outOfBoxCol
        sta GHOST_TGTCOL
        lda #outOfBoxRow
        sta GHOST_TGTROW
        jmp .continue
.normal
;        lda POWER_UP
;        bne .notfrightened
        jsr FrightAI
        jmp .moveghost
.notfrightened        
        ;; lda CHASEMODE
        ;; bne .chasing
        ;; jsr ScatterGhostAI
        ;; jmp .continue
.chasing        
        ;; switch on ghost # to get ai routine
        cpx #4
        bne .ghost3
        jsr Ghost4AI
        jmp .continue
.ghost3        
        cpx #3
        bne .ghost2
        jsr Ghost3AI
        jmp .continue
.ghost2
        cpx #2
        bne .ghost1
        jsr Ghost2AI
        jmp .continue
.ghost1        
        cpx #1
;        beq .animate
        bne .continue
        jsr Ghost1AI
.continue
        jsr PossibleMoves
        cpx #focusGhost
        bne .notfocus
;        Display1 "G",17,GHOST_DIR
.notfocus

        jsr UpdateMotion
        jsr SpecialKeys
;        jsr GhostAsPlayer
.moveghost        
        jsr MoveGhost           ;
;        bcs .animate
;        dec Sprite_move,X
        
        jsr Collisions
.animate
        ;; animate the ghost by changing frames
        dec Sprite_frame,X
        bmi .reset                   ;time to wrap around?
        jmp .loop
.reset
        lda #1                  ;reset frame counter
        sta Sprite_frame,X
        jmp .loop
        
.loopend
        rts
;;; open the door on the ghost box
OpenGhostBox SUBROUTINE
        lda #$20
        sta screen+22*[outOfBoxRow+1]+outOfBoxCol
        lda #BLACK
        sta clrram+22*[outOfBoxRow+1]+outOfBoxCol
        rts
LeaveBox SUBROUTINE
        ;; make sure ghost has proper bitmap
        ;; in case if was the eaten eyes leaving the box
        store16x GHOST,Sprite_src
        ;; instruct the ghost to leave the box
        lda #modeLeaving
        sta Sprite_mode,X
        ;; open the door on the box
        jsr OpenGhostBox
        rts
;;; 
DotEaten SUBROUTINE
        jsr SoundOn
        lda #modeLeaving
        ldy DOTCOUNT
        iny
        sty DOTCOUNT
        cpy #pinkyDots
        beq .pinkyexit
        cpy  #fruit1Dots
        beq .fruit
        cpy #fruit2Dots
        beq .fruit
        cpy #inkyDots
        beq .inkyexit           ;let inky out of the house
        cpy #clydeDots
        beq .clydeexit          ;let clyde out
        rts
.fruit
        jsr Fruit
        rts
.inkyexit
        ldx #inky
        jsr LeaveBox
        rts
.clydeexit
        ldx #clyde
        jsr LeaveBox
        rts
.pinkyexit
        ldx #pinky
        jsr LeaveBox
        rts
;;; return true if character in A is a wall
;;; W2 ( candidate position )
IsWall SUBROUTINE
        pha
        cpx #0
        bne .notpacman
        ;; pacman gets some extra checks
        ;; like to turn sound on and off
        ;; and maybe fruits if I do them are here
        cmp #DOT
        bne .notdot
        saveX
        jsr DotEaten
        resX
        jmp .done
.notdot
        jsr SoundOff
.done        
.notpacman
        pla
        cmp #MW
        rts
        
;;; puta fruit out 
Fruit SUBROUTINE
        rts
;;; display a number in A on screen
;;; bit 7 on will call to reverse characters
;;; X location to display
DisplayNum SUBROUTINE
        sta DSPL_3
        lda #zeroDigit
        sta DSPL_1
        sta DSPL_2
        
        lda DSPL_3
.hundreds
        sta DSPL_3
        sec
        sbc #100
        bcc .tens
        inc DSPL_1
        jmp .hundreds
.tens
        lda DSPL_3
tensloop:
        sta DSPL_3
        sec
        sbc #10
        bcc ones
        inc DSPL_2
        jmp tensloop
ones:
        lda #zeroDigit
        clc
        adc DSPL_3
        sta DSPL_3
        ;; ones is now in S4
        lda DSPL_1
        sta screen,X
        lda #PURPLE
        sta clrram,X
        lda DSPL_2
        inx
        sta screen,x
        lda #PURPLE
        sta clrram,X
        lda DSPL_3
        inx
        sta screen,x
        lda #PURPLE
        sta clrram,X
        rts
;;; update {1} with min of A or {1} store Y, which is the considered 'move'
;;; in {2} if it was the best move so far
        MAC UpdateMinDist
        cmp {1}
        bpl .done
        sta {1}                  ;update min
        lda SPRT_LOCATOR         ;sprt_locator contained a directional indicator
        sta {2}
.done        
        ENDM
;;; load sprite head tile screen position pointer into {2}
;;; X = sprite
        MAC ldSprtHeadPos
        move16x {1},{2}
        endm
        ;; you can pick loc or loc2
        ;; load sprite's tail tile screen position pointer into word {2}
        ;; e.g. ldSprtTailPos Sprite_loc,W1 
        ;; e.g. ldSprtTailPos Sprite_loc2,W1 
        mac ldSprtTailPos
        move16x {1},{2}
#if {1}=Sprite_loc
        lda Sprite_dir,X
#else 
        lda Sprite_dir2,X
#endif        
        clc                     ;16 bit add the sprite_dir
        adc {2}
        sta {2}
        lda {2}+1
        adc #0
        sta {2}+1
        ENDM
;;; convert tiles to pixels
;;; X sprite to convert
;;; {1} source col
;;; {2} source row
;;; {3} output column pixels
;;; {4} output row pixels
;;; X sprite to convert
        MAC TileToPixels
        lda {1}
        asl                     ;mul by 8
        asl
        asl
        sta {3}
        lda {2}
        asl                     ;mul by 8
        asl
        asl
        sta {4}
        lda Sprite_offset2,X
        ldy Sprite_dir,X
        cpy #dirVert
        beq .vert
        cpy #dirHoriz
        beq .horiz
        brk                     ;unknown
.vert
        clc
        adc {4}
        sta {4}
        bcc .done               ;no way carry should ever be set
.horiz
        clc
        adc {3}
        sta {3}
.done        
        ENDM
;;; 
PixelPos SUBROUTINE
#if 0
        ldx #0
        TileToPixels PACCOL,PACROW,S1,S2
        Display1 "T",12,PACCOL
        Display1 "Y",15,PACROW
        Display1 "N",6,S1
        Display1 "M",9,S2
#endif
        Display1 "T",12,PACCOL
        Display1 "Y",15,PACROW
        Display1 "N",6,PACXPIXEL
        Display1 "M",9,PACYPIXEL
        rts
;;; Calculate distance of W1 to target tile
;;; input: W1 candidate sprite position
;;; output: GHOST_COL,GHOST_ROW target tile
;;; output:
;;; S3: X dist + Y dist
CalcDistance SUBROUTINE
        sub16Im W1,screen       ;w1 = offset from screen start, input to divide
        jsr Divide22_16         ;calc row/column by division
;        DisplayDivResults
        lda DIV22_RSLT
        sta GHOST_ROW
        sec
        sbc GHOST_TGTROW
        Abs                     ;absolute value of A
        sta S2                  ;distance to pacman Y
        lda W1                  ;load ghost column
        sta GHOST_COL
        sec
        sbc GHOST_TGTCOL        ;subtract pac column
        Abs                     ;absolute value of A

        ;; add row + columns
        clc
        adc S2
.update        
        sta S3
        
;        Display2 S2,S2
        UpdateMinDist GHOST_DIST,GHOST_DIR
        rts
;;; save some properties of a sprite that will be damaged
;;; during AI calcs
SaveSprite SUBROUTINE
        lda Sprite_offset2,X
        sta SAVE_OFFSET2
        lda Sprite_offset,X
        sta SAVE_OFFSET
        lda Sprite_dir,X
        sta SAVE_DIR
        lda Sprite_dir2,X
        sta SAVE_DIR2
        rts
;;; restore some damaged properties of a ghost sprite
;;; after doing the AI calcs
RestoreSprite SUBROUTINE
        lda SAVE_OFFSET2                  ;restore sprite offset
        sta Sprite_offset2,X
        lda SAVE_OFFSET
        sta Sprite_offset,X
        move16xx Sprite_loc,Sprite_loc2
        lda SAVE_DIR
        sta Sprite_dir,X
        lda SAVE_DIR2
        sta Sprite_dir2,X
        rts
        ;; figure out the ORG offset of a 9x9 tile for a sprite
        ;; depending on it's current orientation ( horiz, or vertical )
        ;; X sprite
        MAC OrgOffsetByDir
        lda #1
        cmp Sprite_dir,X
        beq .done
        lda #22                 ;it's vertical then
.done
        ENDM
        ;; debug if this is the focus ghost
        MAC IfFocus
        cpx #focusGhost
        bne .not
        Display1 {1},{2},S3            ;
.not        
        ENDM
;;; X ghost to check
;;; locals: S3,S4,S0 current min distance
PossibleMoves SUBROUTINE
        lda #noChoice
        sta GHOST_DIST      ;initialize least distance to a big number
        sta GHOST_DIR       ;initialize best move to an invalid move

        jsr SaveSprite
.checkdown
        ;; don't reverse
        lda #motionUp
        cmp Sprite_motion,X
        beq .enddown
        ;; check if we can go down
        lda #motionDown         ;
        sta S1                  ;
        sta SPRT_LOCATOR        ;
        jsr scroll_down         ;
        bcs .enddown
        ;; we could go down
        ldSprtTailPos Sprite_loc2,W1 ;correct
        jsr CalcDistance
        jsr RestoreSprite
        IfFocus "D",13
.enddown        
.checkup                             ;
        ;; don't reverse
        lda #motionDown
        cmp Sprite_motion,X
        beq .endup
        ;; check if we can go up
        lda #motionUp                ;
        sta SPRT_LOCATOR             ;
        jsr scroll_up           ;
        bcs .endup
        ;; we could go up;
        ldSprtHeadPos Sprite_loc2,W1 ;
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "U",9            ;
.endup        
.checkright
        ;; don't reverse
        lda #motionLeft       
        cmp Sprite_motion,X
        beq .endright
        ;; check if we can go right
        scroll_right            ;
        bcs .endright
        ;; we could go right
        ldSprtTailPos Sprite_loc2,W1 ;correct
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "R",1                ;
.endright        
.checkleft
        ;; don't reverse
        lda #motionRight
        cmp Sprite_motion,X
        beq .endleft
        ;; check if we can go left
        scroll_left             ;
        bcs .endleft
        ;; we could go left
        ldSprtHeadPos Sprite_loc2,W1 ;
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "L",5                ;
.endleft        
.done
        rts
;;;  BlinkyRow,pacrow+2,inky target row
;;;  e.g. foo DIV22_RESLT,GHOST1_TGTROW,GHOST_TGTROW
        MAC InkyTargetTile
        lda {1}
        sec
        sbc {2}
        DoubleSigned
        sta {3}
        lda {1}
        sec
        sbc {3}
        sta {3}
        ENDM

        MAC TryMove
        lda MotionTable,Y
        sta GHOST_DIR
        jsr UpdateMotion2
        jsr MoveGhost
        ENDM
;;; ghost running away AI
;;; random turns selected
FrightAI SUBROUTINE
        txa
        pha
        lda Sprite_motion,X
        sta S5
        tsx
        sta S6                  ;save stack pointer
        jsr rand_8
        sta S7
        jsr SaveSprite
.checkdown        
        lda #motionUp
        cmp S5
        beq .enddown
        jsr scroll_down
        bcs .enddown
        ;; we could go down, should we?
        lsr S7
        bcs .done
        lda #motionDown
        pha
        jsr RestoreSprite
.enddown
.checkup        
        lda #motionDown
        cmp S5
        beq .endup
        jsr scroll_up
        bcs .endup
        ;; we could go up, should we?
        lsr S7
        bcs .done
        lda #motionUp
        pha
        jsr RestoreSprite
.endup
.checkleft
        lda #motionRight
        cmp S5
        beq .endleft
        scroll_left
        bcs .endleft
        ;; we could go left, should we?
        lsr S7
        bcs .done
        lda #motionLeft
        pha
        jsr RestoreSprite
.endleft
.checkright
        lda #motionLeft
        cmp S5
        beq .endright
        scroll_right
        bcs .endright
        ;; we could go right, should we?
        lsr S7
        bcs .done
        lda #motionRight
        pha
        jsr RestoreSprite
.endright
        ;; if we got here we didn't take any of our possible moves
        ;; do to the random number
        ;; now we just punt and pick the first one on the stack
        
.done        
        rts
;;; 
ScatterGhostAI SUBROUTINE
        tya                     ;mul by 2
        lsr
        tay
        lda GhosthomeTable,Y
        sta GHOST_TGTCOL
        iny
        lda GhosthomeTable,Y
        sta GHOST_TGTROW
        rts
;;; 
;;; the ghost the runs away when pacman is too close
;;; sue or clyde
;;; opposite quadrants
;;; 
Ghost4AI SUBROUTINE
        ;;  determine distance to pacman
        ;; I should care about whether I load the head or tail pos
        ;; but who cares for this ghost's 'lazy' pursuit mode
        ;;  doesn't seem like it makes a diff
        move16x Sprite_loc,W1
        ;; let blinky calc our target tile
        jsr Ghost2AI            ;blinky's algo
        ;; how far are we away from where blinky would like to be?
        jsr CalcDistance        
;        Display1 "Y",0,GHOST_ROW
;        Display1 "X",3,GHOST_COL
        lda S3
        ;; < N and we are too close, flee
        cmp #9
        beq .tooclose
        bcc .tooclose
        ;; not too close, pursue
        lda #PURPLE
        sta Sprite_color,X
        rts
.tooclose
        ;; run away to opposite quadrant of pacman
        lda #WHITE
        sta Sprite_color,X
        lda PACCOL
        cmp #11
        bcs .paconright
        ;; pac on left
        lda #4
        sta  GHOST_TGTROW
        lda #22-5
        sta GHOST_TGTCOL
        rts
.paconright
        lda #4
        sta GHOST_TGTROW
        lda #5
        sta GHOST_TGTCOL
        rts
;;;
;;; ghost 1
;;; draws a vector from his initial target tile ( 2 in front of pacman )
;;; to the current position  of blink
;;; doubles the length of the vector
;;; and uses the result as his target tile
Ghost1AI SUBROUTINE

        ;; our initial target it 2 in front of pacman
        ;; we'll leverage ghost 3's work for us
        ;; his target tile was 4 in front of pacman
        saveX

        ;; blinky ( ghost 2 ) calculated just before us, so we'll use
        ;; his position
        ldx #2
        move16x Sprite_loc2,W1
        sub16Im W1,screen ;w1 = offset from screen start, input to divide
        jsr Divide22_16
        lda DIV22_RSLT          ;blinky's row
        ;; determine the Y distance between blinky and our target tile
        ;; then , double it
        InkyTargetTile DIV22_RSLT,GHOST1_TGTROW,GHOST_TGTROW
        InkyTargetTile W1,GHOST1_TGTCOL,GHOST_TGTCOL

;        Display1 "Y",22,GHOST_TGTROW
;        Display1 "X",25,GHOST_TGTCOL
        ;; should have our final target tile
        
        resX
        
        rts
;;; 
;;; simple hot pursuit ( Blinky )
;;; 
Ghost2AI  SUBROUTINE
        lda PACCOL
        sta GHOST_TGTCOL
        lda PACROW
        sta GHOST_TGTROW
        rts
;;; ghost 1 AI ( Pinky )
Ghost3AI SUBROUTINE
        lda Sprite_dir
        ldy Sprite_motion
        cmp #1
        beq .horiz
        cmp #22
        beq .vert
        brk
.horiz                          ;pacman is horizontal
        lda PACROW
        sta GHOST_TGTROW
        sta GHOST1_TGTROW
        cpy #motionLeft
        bne .right
        ;; left
        lda PACCOL
        sec
        sbc #4
        sta GHOST_TGTCOL
        sbc #2
        sta GHOST1_TGTCOL
        rts
.right
        lda PACCOL
        clc
        adc #4
        sta GHOST_TGTCOL
        sta GHOST1_TGTCOL
        rts
.vert                           ; pacman is going vertical
        lda PACCOL
        sta GHOST_TGTCOL
        sta GHOST1_TGTCOL
        cpy #motionUp
        bne .down
        ;;  going up
        lda PACROW
        sec
        sbc #2
        sta GHOST1_TGTROW
        sbc #2
        sta GHOST_TGTROW
        rts
.down
        lda PACROW
        clc
        adc #2
        sta GHOST1_TGTROW
        adc #2
        sta GHOST_TGTROW
        rts


;;; W1 contains dividend ( word )
;;; w1 contains remainder on exit
;;; DIV22_RSLT contains result up to 5 bits of precision
;;; 
;;; in the context of the game screen
;;; DIV22_RSLT is the row, and A is the column
Divide22_16 SUBROUTINE
        txa
        pha
        
        lda #$00
        sta DIV22_RSLT      ;Init the result variable
        ldx #0
.loop
        move16x Div22Table,DIV22_WORK
        cmp16 W1,DIV22_WORK
        bcc .1
        sub16 W1,DIV22_WORK
.1
        rol DIV22_RSLT
        inx
        cpx #5
        beq .done
        bne .loop
.done
        pla
        tax

        rts
        ;; {1}=border {2}=background
        MAC SetBorderAndBackgroundColor
        lda #[{1}&%11]|[{2}&%f0]
        sta 36879
        ENDM

;;; 
;;; Service PACMAN, read joystick and move
;;; 
Pacman SUBROUTINE
        ldx #0

        MyTurn2 PacManTurn
        ;;not our turn to move
        rts
PacManTurn
        lda #cornerAdv
        sta CORNER_SHAVE        ;pac get +1 bonus on corners
        
        stx MOVEMADE
        readJoy
#ifnconst _SLOWPAC        
        cmp LASTJOYDIR          ;same as last joystick reading
        beq .uselast            ;then use last reading
#endif       
        and #$bc
        eor #$bc
        beq .uselast
        lda LASTJOY
        jmp .process
.uselast
#ifconst _SLOWPAC        
        rts
#endif        
        ldy MOVEMADE            ;if we made a move already then exit
        beq .cont
        rts
.cont        
        ldy #1
        sty MOVEMADE            ;indicate we made a move
        lda LASTJOYDIR
        sta LASTJOY
        
.process
        ldy #0
        sty S1 ;eat dots from head tile
;        sta screen+1            ; debug char on screen
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
        and #JOYR
        beq .right
        rts
.fire
        rts
.left
        store16 PAC_L1,W3
        lda #motionLeft
        sta Sprite_motion
        scroll_left
        bcs .uselast            ;couldn't move, use last reading
        jmp .moveok
.up
        store16 PAC_UP1,W3
        lda #motionUp
        sta Sprite_motion
        jsr scroll_up
        bcc .moveok
        jmp .uselast
.down
        lda #1
        sta S1                  ;eat dots from tail
        store16 PAC1D,W3
        lda #motionDown
        sta Sprite_motion
        jsr scroll_down
        bcc .moveok
        jmp .uselast
.right
        lda #1
        sta S1                  ;eat dots from tail
        store16 PAC1,W3    ;
        lda #motionRight
        sta Sprite_motion
        scroll_right
        bcc .moveok
        jmp .uselast
.moveok

        lda LASTJOY
        sta LASTJOYDIR
        move16 W3,Sprite_src
        jsr Animate
        rts
;;; 
;;; X ghost we are checking for collision
Collisions SUBROUTINE
        CalcGhostRowCol
        ;; special directions for ghosts leaving box
        lda Sprite_mode,X
        cmp #modeLeaving        ;check if we are exiting box
        beq .enter_exit
        cmp #modeEaten
        beq .enter_exit
        bne .notspecial
.enter_exit        
        ;; we are entering or exiting the box
        ;; if we are right ABOVE the box
        
        ldy #outOfBoxCol        ;check target column
        cpy GHOST_COL
        bne .notspecial
        
        ldy #outOfBoxRow        ;check target row
        cpy GHOST_ROW
        beq .special
        iny                     ;check if IN the box
        iny                     ;which is two tiles down
        cpy GHOST_ROW
        bne .notspecial
        ;; in the box
        cmp #modeEaten
        bne .notspecial
        lda #motionUp           ;set upward, out of box
        sta Sprite_motion,X     ;store it on sprite
        jsr LeaveBox            ;send em right back out
        jmp .notspecial
.special                        ;above the box
        cmp #modeLeaving
        bne .eaten
        CloseGhostBox
        sta Sprite_back2,X
        lda #modeOutOfBox
        sta Sprite_mode,X
        jmp .notspecial
.eaten
        jsr OpenGhostBox
        lda #motionDown
        sta Sprite_motion,X
        jsr scroll_down
.notspecial
        TileToPixels GHOST_COL,GHOST_ROW,S1,S2
        lda S1
        sec
        sbc PACXPIXEL
        Abs
        cmp #4
        bcs .done

        lda S2
        sec
        sbc PACYPIXEL
        Abs
        cmp #4
        bcs .done
        ;; collision between pacman and ghost
        lda POWER_UP
        bne .ghost_eaten
        ;; pacman eaten
;        brk
        jmp .done
.ghost_eaten
        lda #modeEaten
        sta Sprite_mode,X
;        lda #WHITE
;        sta Sprite_color,X
        store16x BIT_EYES,Sprite_src
.done        
        ;; GHOST_ROW,GHOST_COL
        rts
;;; animate a sprite by changing its source frames
Animate SUBROUTINE
        lda PACFRAMED
.start        
        clc
        adc Sprite_frame
        bmi .reverse
        cmp #4
        beq .reverse
        sta Sprite_frame
        rts
.reverse
        lda #$ff
        eor PACFRAMED
        ora #1
        sta PACFRAMED
;        jmp .start
        rts
        
        ;; load the currently rendered tile for a sprite
        ;; X sprite in question
        mac loadTile
        lda Sprite_tile,X
        ldy #0
        cpy Sprite_page         
        bne .tileSet0
        clc
        adc #2
.tileSet0
        endm
;;; uses W1,W2,W3
blargo SUBROUTINE

SPRT_IDX set S3                 ;loop counter.  The sprite to check against
SPRT_CUR set S2                 ;current sprite

        lda #NOTCOPY
        sta Sprite_sback,X
        sta Sprite_sback2,X
        ldSprtHeadPos Sprite_loc2,W1 ;current sprite head into W1
        ldSprtTailPos Sprite_loc2,W2 ;current sprite tail into W2
        
        stx SPRT_CUR            ;save current sprite idx
        lda #SPRITES
        sta SPRT_IDX            ;loop counter , start at SPRITES

        jmp .loop
;;; before we leave, put screen background tiles into any compositing backgrounds
;;; that weren't already filled in
.done
        ldy #0
        ldx SPRT_CUR          
        lda Sprite_sback,X
        cmp #NOTCOPY            ;is head tile filled in?
        bne .checktail
        lda (W1),Y              ;nope, fill it with screen
#if _debug        
        cmp #MW
        bne .ok
;        brk                     ;we just loaded a mazewall, something is wrong
.ok        
#endif
        sta Sprite_sback,X
.checktail
        lda Sprite_sback2,X
        cmp #NOTCOPY            ;is tail tile filled in?
        bne .alldone
        lda (W2),Y              ;nope, fill it with screen
#if _debug        
        cmp #MW
        bne .ok2
;        brk                     ;we just loaded a mazewall, something is wrong
.ok2        
#endif
        sta Sprite_sback2,X
.alldone        
        rts
        
.loop
        dec SPRT_IDX
        lda SPRT_IDX
        cmp SPRT_CUR ;(a-m)
        bmi .done    ;do not compare sprites < SPRT_CUR for collisions
        
        ldx SPRT_IDX
        ;; if we are checking for collisions against ourselves, we use
        ;; use current loc, and not loc2 ( I forget why , need to comment )
        ldSprtHeadPos Sprite_loc,W3 ;SPRITE_IDX's current head position into W3
        cpx  SPRT_CUR           ;are we checking against ourselves?
        beq .ourselves          ;yes, then W3 is good to go
        ldSprtHeadPos Sprite_loc2,W3 ;not ourselves, W3 =  SPRT_IDX's new position
.ourselves        
        ;; check if we hit SPRT_IDX's head with our head
        cmp16 W1,W3             ;our head == sprite_idx's head
        bne .not_head2head
        jsr head2head
.not_head2head        
        ;; check SPRT_IDX's head for collision with our tail
        cmp16 W2,W3             ;out tail == sprite_idx's head
        bne .not_tail2head
        jsr tail2head
        ;; we check oue head and tail against oncoming sprites heads , now
        ;; it's time to check our head n tail against oncoming sprites tails
.not_tail2head        
        ldSprtTailPos Sprite_loc,W3 ;S3 sprite's current tail position into W3
        cpx  SPRT_CUR           ;are we checking against ourselves
        beq .ourselves2         ;yes, then W3 is good to go
        ldSprtTailPos Sprite_loc2,W3 ;not ourselves, load W3 with S3 sprites's new position
.ourselves2        
        cmp16 W1,W3             ;our head == sprite_idx's tail?
        bne .not_head2tail
        jsr head2tail 
.not_head2tail        
        cmp16 W2,W3             ;our tail  == sprite_idx's tail?
        bne .not_tail2tail
        jsr tail2tail
.not_tail2tail        
        jmp .loop               ;all possible collisions checked
        brk
        
;;; handle tail to head collisions
;;; X is the indexed sprite
tail2head SUBROUTINE
        cpx SPRT_CUR
        beq .ourselves
        ;; composite the colliding sprites tail onto our head
        loadTile2
        ldy SPRT_CUR
        sta Sprite_sback2,Y     ;store colliding sprite tile as current sprite's head tile 'background'
        rts
.ourselves
        ;; this would happen when scrolling left, for example
        lda Sprite_sback2,X    ;load tail background
        cmp #NOTCOPY           ;is it still 'unset'
        bne .done              ;no, it's been set already, we are done
        ;; tail hasn't been set, let's copy our head 'playfied' background into it
        lda Sprite_back,X       ;load head playfield background
        sta Sprite_sback2,X     ;save into tail 'compositor' tile
;        brk
.done        
        rts
        
head2head SUBROUTINE
        cpx SPRT_CUR
        beq .ourselves
        loadTile2
        ldy SPRT_CUR
        sta Sprite_sback,Y
        rts
.ourselves
        lda Sprite_sback,X
        cmp #NOTCOPY
        bne .done
        lda Sprite_back,X
        sta Sprite_sback,X
.done        
        rts
        ;; rendering 0 and had a collision with 1
        ;; ( the ghost is moving over pacman )
        ;; X the indexing sprite we are checking against
head2tail SUBROUTINE
        cpx SPRT_CUR
        beq .ourselves

        loadTile2
        clc
        adc #01

        ldy SPRT_CUR
        sta Sprite_sback,Y
        rts
.ourselves
        lda Sprite_sback,X
        cmp #NOTCOPY
        bne .done
        lda Sprite_back2,X
        sta Sprite_sback,X
.done        
        rts
tail2tail SUBROUTINE
        cpx SPRT_CUR
        beq .ourselves
        loadTile2
        clc
        adc #01
        ldy SPRT_CUR
        sta Sprite_sback2,Y
        rts
.ourselves
        lda Sprite_sback2,X
        cmp #NOTCOPY
        bne .done
        lda Sprite_back2,X
        sta Sprite_sback2,X
.done
        rts
;;; handle tunnel left side
DecrementPos SUBROUTINE
        cmp16Im W2,[tunnelRow*22]+tunnelLCol+$1e00
        bne .done
        store16 [tunnelRow*22]+tunnelRCol+$1e00,W2
        rts
.done
        Dec16 W2
        rts
;;; handle tunnel right side
IncrementPos SUBROUTINE
        cmp16Im W2,[tunnelRow*22]+tunnelRCol+$1e00
        bne .done
        store16 [tunnelRow*22]+tunnelLCol+$1e00,W2
        rts
.done
        Inc16 W2
        rts
scroll_horiz SUBROUTINE
        lda Sprite_dir,X        ;get our current orientation
        cmp #1                  ;are we already horizontal
        beq .ok                 ;ok to move horizontal
        jsr changehoriz         ;no, switch to horizontal
        bcc .ok                 ;change successful
        rts                     ;we could't change to horizontal
.ok
        ldy Sprite_offset,X     ;get our current pixel offset
        cpy END_SCRL_VAL        ;are we at the end of our tiles?
        bne .draw               ;no, we can smooth scroll
.course                         ;course scroll

        move16x Sprite_loc,W2   ;screen location to W2
        lda SCRL_VAL            ;load scroll direction 
        bmi .left               ;branch to left code
        ;; going right
        lda #0                  ;push potential new sprite offset
        pha                     ;save it on stack
        jsr IncrementPos        ;move over to right one tile
        ldy #01                 ;
        lda (W2),Y              ;check for wall at pos + 2
        jsr IsWall              ;remember we are 2 tiles wide
        bne .continue           ;there is no wall, we can move
.cantmove
        sec                     ;can't move, return false
        pla                     ;pop the single arg we pushed for our own use
        rts
.left
        lda #8
        pha
        jsr DecrementPos
        ldy #0
        lda (W2),Y
        jsr IsWall
        beq .cantmove
.continue
        move16x2 W2,Sprite_loc2  ;save the new sprite screen location
        pla                      ;pull new sprite offset from the stack
        tay
.draw
        tya
        clc
        adc SCRL_VAL
        sta Sprite_offset2,X
        clc
        rts
;;; attempt to turn the dot eating sound back on
SoundOn SUBROUTINE
        lda #8
        sta 36879
        ldx #3
        UnHaltTrack
        ldx #0
        rts
;;; attempt to turn the dot eating sound off
SoundOff SUBROUTINE
        lda #13
        sta 36879
        ldx #3
        HaltTrack
        ldx #0
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
    bne .loop
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
;#if _debug
#if 0        
;;; X sprite background tiles to dump
dumpBack SUBROUTINE
        txa
        asl
        tay
        lda Sprite_back,X      ;input to mergeTile
        sta screen+4,Y
        lda Sprite_back2,X
        sta screen+5,Y
        rts
dumpBack2 SUBROUTINE
        txa
        asl
        tay
        lda Sprite_sback,X      ;input to mergeTile
        sta screen+9,Y
        lda Sprite_sback2,X
        sta screen+$A,Y
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
;        sta screen+9,Y
        mergeTile               ;font address of underneath tile into W4
        move16 W4,W6

        pla
        tay
        lda Sprite_sback2,X
;        sta screen+$A,Y
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
        jsr changevert          ;offset = 8 if we successfully change orientation
        bcc .00
        rts
.00
        ldy Sprite_offset,X
        bne .fine               ; > 0 then fine scroll
.course                         ; course scroll

        move16x Sprite_loc,W2   ; screen location to W2
        sub16Im W2,#22            ; check up one tile
        ldy #0
        lda (W2),Y
        jsr IsWall
        bne .continue
.cantmove        
        sec
        rts
.continue
        lda #1
        move16x2 W2,Sprite_loc2  ;save the new sprite screen location
        ldy #8                  ;reset fine scroll offset
.fine

        dey
        tya
        sta Sprite_offset2,X
.done
        clc
        rts

;;; change orientation to horizontal
;;; X sprite to attempt change on
;;; SPRT_LOCATOR direction to change to ( 22=left 24=right)
;;; output: sprite_loc2 contains new head position if successful
;;; the ORG tile is picked such that test for a wall character
;;; can use the same offset no matter what tile the sprite
;;; is living in i.e. the head of the tail
;;; for example: in the diagram below ( lower left )
;;; when the sprite is in the tail tile, to check for a wall
;;; going to the right is ORG+24
;;; in the next case, sprite living in the head tile
;;; ORG+24 also checks for a wall when moving right
;;; 
;;; destruction S2,S3,W2,W1
;;; -------------------------------------------------------
;; let L,H be left tile and right tile of pacman, let O be the origin
;; start                   start
;;   #.#     #.#     #.#   ##O###  ######  ######
;; ##OL##  ###.##  ###.##  #..L..  #.LH..  #..LH.
;; ...H.   ..LH.   ...LH   ###H##  ###.##  ###.##
;; ######  ######  ######    #.#     #.#     #.# 
;;; 
changehoriz SUBROUTINE
        move16x Sprite_loc,W1
        lda #1                 ; offset to blank tile in endtile case
        sta S3                  ;-offset to ORG in endtile case
        lda Sprite_offset,X     ;where is sprite offset
        cmp CORNER_SHAVE        ;with shave dist of beg tile?
        bcc .begtile            ;less then shave
        beq .begtile            ;= to shave
        clc
        adc CORNER_SHAVE        ;if offset >= 8-shave dist
        cmp #8                  ;then ok to corner at end
        bcs .endtile
.failed                         ;can't change direction yet
        sec
        rts
.begtile                        ;at beginning of tile
        lda #23                 ;update offset to ORG
        sta S3                  ;for begtile case

.endtile                        ;end of tiles branch
        subxx W1,W2,S3          ;generate ORG address in W2
        
        ldy SPRT_LOCATOR
        lda (W2),Y
        jsr IsWall
        beq .failed             ;we hit a wall, abort move
        
        lda #1                  ;change direction to horiz
        sta Sprite_dir2,X

        lda #0                  ;assume moving right
        sta Sprite_offset,X     ;sprite offset = 0

        lda #23                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving right

        lda SPRT_LOCATOR        ;moving right?
        cmp #motionRight
        beq .isright            ;branch moving right

        lda #8                  ;going left, sprite offset = 8
        sta Sprite_offset,X

        lda #22                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving left
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
        sty S3                  ;offset to ORG in endtile case
        lda Sprite_offset,X     ; where is sprite offset
        cmp CORNER_SHAVE        ; within shave dist of 0?
        bcc .begtile
        beq .begtile            ; at beginning of tile
        clc
        adc CORNER_SHAVE        ; are we within shave distance
        cmp #8                  ; of end of tile?
        bcs .endtile
.failed ; we can't change directions  in middle of tile
        sec
        rts
.begtile                        ;at beginning of tile
        ldy #23
        sty S3                  ;update ORG offset
.endtile                        ;end of tiles branch
        subxx W1,W2,S3          ;generate ORG address in W2
        
        ldy S1
        lda (W2),Y
        jsr IsWall
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
;;; carry is set on return if move is not possible
;;; destroys: S1,S3,W2,W1
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
        ldy #44                 ;check one tile down from tail
        lda (W2),Y
        jsr IsWall
        bne .continue
.cantmove
        sec                     ;indicate failure
        rts
.continue        

        addxx W2,W1,#22
        move16x2 W1,Sprite_loc2  ;save the new sprite screen location
        ldy #0                  ;reset fine scroll offset
.fine
;        brk
        iny
        tya
        sta Sprite_offset2,X
.done
        store16 PAC1D,W1
        clc
        rts

        
        MAC DisplayBCDDigit
        cmp #10
        bmi .numbers
        sec
        sbc #9                  ;cuz 0 is the '@' char in vic
        ora #$80
        jmp .onscreen
.numbers
        clc
        adc #zeroDigit
.onscreen
        sta screen,X
        lda #PURPLE
        sta clrram,X
        ENDM
;;; 
;;; Display a BCD number
DisplayBCD SUBROUTINE
        lda BCD
        lsr
        lsr
        lsr
        lsr
        DisplayBCDDigit
        lda BCD
        and #$0f
        inx
        DisplayBCDDigit
        rts
#if 0        
;;; binary to bcd
Bin2Hex SUBROUTINE        
.BINBCD8
        txa
        pha
        
	SED                     ; Switch to decimal mode
	LDA #0                  ; Ensure the result is clear
	STA BCD+0
	STA BCD+1
	LDX #8                  ; The number of source bits
        
.CNVBIT
	ASL BIN_IN		; Shift out one bit
	LDA BCD+0               ; And add into result
	ADC BCD+0
	STA BCD+0
	LDA BCD+1               ; propagating any carry
	ADC BCD+1
	STA BCD+1
	DEX                     ; And repeat for next bit
	BNE .CNVBIT
	CLD                     ; Back to binary

        pla
        tax
        rts
#endif        
; returns pseudo random 8 bit number in A. Affects A. (r_seed) is the
; byte from which the number is generated and MUST be initialised to a
; non zero value or this function will always return zero. Also r_seed		
; must be in RAM, you can see why......

rand_8 SUBROUTINE
	LDA	r_seed		; get seed
	ASL			; shift byte
	BCC	no_eor		; branch if no carry

	EOR	#$CF		; else EOR with $CF
no_eor
	STA	r_seed		; save number as next seed
	RTS			; done
;;; 
;;; S5 * S6 output ( 16 bit ) W5
;;; 
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

        org $1c00
        ds 8*4,0
        ;; regular eating dot
        dc.b 0
        dc.b 0
        dc.b 0
        dc.b 24
        dc.b 24
        dc.b 0
        dc.b 0
        dc.b 0
        ;; maze wall
        dc.b $ff
        dc.b $ff
        dc.b $ff
        dc.b $ff
        dc.b $ff
        dc.b $ff
        dc.b $ff
        dc.b $ff
        ;; power pellet
        dc.b 0
        dc.b 24
        dc.b 60
        dc.b 60
        dc.b 60
        dc.b 60
        dc.b 24
        dc.b 0
        ;; horizontal wall
        dc.b %00000000
        dc.b %00000000
        dc.b %11111111
        dc.b %00000000
        dc.b %00000000
        dc.b %11111111
        dc.b %00000000
        dc.b %00000000
        ;; vertical wall
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        dc.b %00100100
        ;; eyes
BIT_EYES
        dc.b %00000000
        dc.b %00000000
        dc.b %00000000
        dc.b %11100111
        dc.b %10100101
        dc.b %11100111
        dc.b %00000000
        dc.b %00000000
        ;; 
        dc.b %00000000
        dc.b %00000000
        dc.b %00000000
        dc.b %11100111
        dc.b %11100111
        dc.b %11100111
        dc.b %00000000
        dc.b %00000000
        
        ;; now the 4 ghosts occupy this area
        ;;  need to clean this up a bit
        
        ds 4*8,0
        ;; eyes
        
        
;;; BEGIN custom character set
;    org mychars                 ;
;        dc.b $ff
#if 0
;;; scratch text for debugging thoughts
;; 0 @
;; 1 a
;; 2 b
;; 3 c
;; 4 d
;; 5 e
;; 6 f
;; 7 g
;; 8 h
;; 9 i
;; a jp
;; b k
;;; example of accessing stack local variable passed C style
;; lda #$ab
;; pha
;; tsx
;; inx
;; lda $100,X
;; sta $0
;;; for debugging, here are the keyboard read codes for the ghost control
;;; up 9,down 26,left 17,right 18 a,d w,x
why tile scrolling works
we allow going to 8 or 0 which always allows to make it to the end of a tile set
so if you are heading towards a wall, you will always be able to touch it

I experimented with allowing pacman to cut corner by decreasing the number
at which we allow the changevert or horz to operate, and it seems to work

ghost AI

        does the tile distance logic need to be aware of where pacman the sprite is
        withing the tiles?
        I'm using the same routines we scroll with, scroll_left or scroll_right
        they will take care of knowing whether the move could take place
        if you are at the end of a range or not

        lets get the tunnel working
        
        
        ghost box
        row 11
        col 9

        have a mode where their AI isn't running
        then when it's time to get out of the box
        set the target tile

        there are 171 dots in our maze
        
#endif        