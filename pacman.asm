;; DOT
;; POWER
;; LEFT
;; RIGHT
;; TOP
;; BOTTOM
;; H WALL
;; V WALL
;;; enums for reading the compressed maze

    org $0400
    processor 6502
_debug    equ 1            ; true for debugging
voice1    equ 36874        ; sound registers
voice2    equ 36875
voice3    equ 36876
volume    equ 36878
;screen    equ $1000        ; screen ram
screen    equ $1e00        
;clrram    equ $9400        ; color ram for screen
clrram    equ $9600        ; color ram for screen
clroffset equ $78          ;offset from screen to color ram

VICRASTER equ $9004        
VICSCRN   equ $9005               ;vic chip character generator pointer
VIA1DDR   equ $9113
VIA2DDR   equ $9122          ; ?
JOY0      equ $9111
JOY0B     equ $9120          ; output register B VIA #2
JOYUP     equ $4             ; joy up bit
JOYDWN    equ $8             ; joy down
JOYL      equ $10            ; joy left
JOYT      equ $20            ; joy fire
JOYR      equ $80            ; joy right bit

jiffyh    equ $a0          ; jiffy clock lsb - msb
jiffym    equ $a1
jiffyl    equ $a2

chrom1    equ $8000        ; upper case character rom
chrom2    equ $8400        ; upper case reversed
chrom3    equ $8c00        ; upper lower reversed
chrom4    equ $8800        ; upper lower

;; vic-I chip registers
chrst           equ $9003        ; font map pointer
;mychars         equ $1400        ; my font map
mychars         equ $1c00        
charcnt         equ $800
zeroDigit       equ 48 | $80     ;zero digit character

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

SPRITES      equ 3             ;count of sprites in system (1 based)
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
DIV22_WORK      equ 12          ;word
;;;                 13
DIV22_RSLT      equ 14          ;div22 result
SPRITEIDX       equ 15        ;sprite index for main loop
CSPRTFRM        equ 16        ; number of frames in the currently processing sprite
PACFRAMEN       equ 18        ; byte: index of pac frame
PACFRAMED       equ 19        ;pacframe dir
NXTSPRTSRC      equ 20        ;when moving a sprite, the next 'set' of source bitmaps
DSPL_1          equ 21        ;used by DisplayNum routine
DSPL_2          equ 22
DSPL_3          equ 23
GHOST_DIST      equ 24          ;best distance for current ghost AI calcs
GHOST_DIR       equ 25          ;best move matching GHOST_DIST
;;; e.g. if pacman successfully moves up, then switch to PAC_UP1 set of source 
S5              equ $30
S6              equ $31        
S7              equ $43
W5              equ $32
W6              equ $44
PACROW          equ $47         ;current pacman row
PACCOL          equ $48         ;current pacman column
;;; sentinal character, used in tile background routine
;;; to indicate tile background hasn't been copied into _sback yet
NOTCOPY         equ $fd      
        
;;;offset for normalizing a 9x9 sprite movement block to the upper left block
;;; used by the sprite orientation changing routines
SPRT_LOCATOR    equ $47
;value that indicates end of smooth scrolling
END_SCRL_VAL    equ $48
;;;amount to increase or decrease sprite offset
;;; used by scroll_horiz
SCRL_VAL        equ $49
LASTJOY         equ $4a
LASTJOYDIR      equ $4b         ;last joy reading that had a switch thrown
MOVEMADE        equ $4c         ;true if last pacman move was successful
VV              equ $02         ;testing, voice 2
;;
;; misc constants
;;
PACL            equ $00            ; pacman char number

GHL             equ $08            ; ghost char number
GH1L            equ $0c
DOT             equ $04
WALLCH          equ $05
MW              equ $05            ;maze wall character
PWR             equ $06
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
        MAC cmp16Im
        lda {1}
        cmp #[{2}] & $ff    ; load low byte
        bne .done
        lda {1}+1
        cmp #{2} >> 8     ; load high byte
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
    dc.b %11010010
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
    dc.b %01001001
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
WALL
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111
    ds 1,%11111111

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
;;;
;;; there are 4 sets of frames
;;; for each sprite
;;; the order of sets is left,right,top,bottom
;;; 
;;; debugging notes: 22*5+5 is at an intersection
;;; for testing up/down/left/right transitions
;------------------------------------
Sprite_page     dc.b 0        
Sprite_loc      DC.W 0,0,0,0,0    ;screen loc
Sprite_loc2     DC.W screen+22*2+8 ,screen+22*5+3,screen+22*7+5,0,0    ;new screen loc
Sprite_back     dc.b 0,0,0,0,0           ;background char value before other sprites are drawn
Sprite_back2    dc.b 0,0,0,0,0           ;static screen background
Sprite_sback    dc.b 0,0,0,0,0 ;current screen background ( might include some other sprite tile that was laid down )
Sprite_sback2   dc.b 0,0,0,0,0        
Sprite_tile     dc.b PACL,GHL,GH1L,0,0         ;foreground char
Sprite_src      dc.w PAC1,GHOST,GHOST,0,0       ;sprite source bitmap
Sprite_frame    dc.b 0,0,0,0,0              ;animation frame of sprite as offset from _src
;;; sprite chargen ram ( where to put the source bmap )
Sprite_bmap     dc.w mychars+(PACL*8),      mychars+(GHL*8)      ,mychars+(GH1L*8),0,0    
Sprite_bmap2    dc.w mychars+(PACL*8)+(2*8),mychars+(GHL*8)+(16) ,mychars+(GH1L*8)+(16),0,0
Sprite_motion   dc.b 1,1,$EA,1,1        
Sprite_dir      dc.b 1,1,22,1,1  ;sprite direction 1(horiz),22(vert)
Sprite_dir2     dc.b 1,1,22,1,1  ;sprite direction 1(horiz),22(vert)    
Sprite_offset   dc.b 0,4,0,0,0  ;sprite bit offset in tiles
Sprite_offset2  dc.b 0,4,0,0,0  ;sprite bit offset in tiles
;;; 
;;; division table for division by 22
Div22Table dc.w [22*16],[22*8],[22*4],[22*2],[22*1]
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
        lda #clroffset          ;W1 now = color ram location
        adc W1+1
        sta W1+1
        lda #WHITE
        ldy #0
        sta (W1),Y
        ldy Sprite_dir,X
        sta (W1),Y
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
        
;;; TODO: the screen backing tiles needs to check to 'upcoming' sprite tile positions
;;; X = sprite to move
;;; uses W2,S2
drwsprt1 SUBROUTINE
        move16x Sprite_loc,W1

        ;; figure out which set of tiles we should be rendering
        ;; we 'page flip' between tiles for speed
        loadTile                ;upcoming tile into A
        
        ldy #0   
        sta (W1),Y
        clc
        adc #01                 ;inc to right side tile

;        jmp .skip
        ldy Sprite_dir,X
.skip        
        sta (W1),Y              ;lay down the tile
        clc
        lda #clroffset          ;W1 now = color ram location
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
;        move16x Div22Table,DIV22_WORK
#if 0        
        ;; lda #255
        ;; jsr DisplayNum
        ;; brk
        store16 screen+22*5+8,W1
        sub16Im W1,screen
        jsr Divide22_16
        brk
        ldx #0
        jsr DisplayNum
        ldx #4
        lda S1
        jsr DisplayNum
        brk
        ;; lda #21
        ;; jsr Stuff
        ;; brk
#endif        
        lda #pacframes
        sta PACFRAMEN
        lda #1
        sta PACFRAMED
        lda #$ff
        sta LASTJOYDIR
    cli                         ; enable interrupts so jiffy clock works
    lda #8
    sta 36879                   ; border and screen colors
    lda #$0f                    ; max
    sta volume                  ; turn up the volume
    lda #0
    sta $9113                   ; joy VIA to input
    jsr copychar                ; copy our custom char set
    store16 PDOT,W1             ; copy(dot,character[1]);
    store16 mychars+[8*DOT] ,W2 ;
    jsr loadch                  ;
    store16 PPWR,W1             ; copy(power,charcters[2]);
    store16 mychars+[8*PWR],W2  ;
    jsr loadch                  ;
    store16 WALL,W1
    store16 mychars+[8*WALLCH],W2
    jsr loadch
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
        ldx #255
.iloop        
        lda VICRASTER           ;load raster line
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
        sta SPRITEIDX
.eraseloop
        dec SPRITEIDX
        bmi .background
        ldx SPRITEIDX
        jsr erasesprt
        jmp .eraseloop

.background
        lda #SPRITES
        sta SPRITEIDX
.backloop        
        dec SPRITEIDX
        bmi .draw
        ldx SPRITEIDX
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
        sta SPRITEIDX
.drawloop
        dec SPRITEIDX                  ;for i = sprites to 0 , i--
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
        ;; lda #01
        ;; eor Sprite_page         ;let everyone know we are on the other tile page
        ;; sta Sprite_page
        
;        jsr WaitFire
        jsr Pacman
        jsr Ghost
        lda #SPRITES            ;for i = sprites to 0, i--
        sta SPRITEIDX
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
    store16 chrom1 ,W2
    store16 mychars,W3
    store16 charcnt,W1

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
        lda #$20                ;default to space
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
;;; animate a ghost back and forth
Ghost SUBROUTINE
;        rts
        
        ldx #1                  ;work with sprite 0
;        scroll_right
        moveS
        ldx #2

        txa
        pha
        
        jsr GhostAI
        jsr PossibleMoves
        ldx #10
        lda S3
        jsr DisplayNum
        lda S4
        ldx #14
        jsr DisplayNum
        
        pla
        tax

        lda 197
        cmp #26
        beq .down
        cmp #9
        beq .up
        jmp .continue
.up
        jsr scroll_up
        jmp .continue
.down
        jsr scroll_down         ;
        jmp .continue
;        moveD                   ;
.continue
        ;; animate the ghost by changing frames
        lda #1                  ;add one to current frame
        clc
        adc Sprite_frame,X
        sta Sprite_frame,X
        cmp #2                  ;time to wrap around?
        beq .reset
        rts
.reset
        lda #0                  ;reset frame counter
        sta Sprite_frame,X

        ;; calll the ghost AI routine
        rts

	;;  pseudo logic for ghosts
	;; if at intersection
	;; determine possible moves positions
	;; noting that you can't ever reverse direction
	;; calculate distance to pacman-target-tile for each possible move
	;; pick minimum distance
	;; make move
	;; return true if charcter in A is a wall
IsWall SUBROUTINE
        cmp #MW
        rts
;;; display a number in A on screen
;;; bit 7 on will call to reverse characters
;;; {1} location to display
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
        lda SPRT_LOCATOR
        sta {2}
.done        
        ENDM
;;; 
;;; DIV22_RSLT ghost row
;;; W1 ghost column
;;; W2 ORG tile of 'source' sprite to use in distance calculation
;;; Y = offset from W2
;;; output:
;;; S2: Y dist
;;; S3: X dist
CalcDistance SUBROUTINE
        sub16Im W1,screen       ;w1 = offset from screen start, input to divide
        jsr Divide22_16         ;calc row/column by division
        lda DIV22_RSLT
        sec
        sbc PACROW
        sta S2                  ;distance to pacman Y
        lda S0                  ;load ghost column
        sec
        sbc PACCOL              ;subtract pac column
        ;; add row + columns
        clc
        adc S2

        UpdateMinDist GHOST_DIST,GHOST_DIR
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
;;; X ghost to check
;;; locals: S3,S4,S0 current min distance
PossibleMoves SUBROUTINE
        lda $ff
        sta GHOST_DIST                  ;initialize least distance to a big number
        sta GHOST_DIR                  ;initialize best move to an invalid move
        
        lda Sprite_offset,X     ;save sprite offset
        sta Sprite_offset2,X

        lda #24
        sta SPRT_LOCATOR
        ;; check if we can go right
        jsr changehoriz
        bcs .checkleft
        ;; we could go right
        move16x Sprite_loc2,W1
        jsr CalcDistance

        lda Sprite_offset2,X                  ;restore sprite offset
        sta Sprite_offset,X
        move16xx Sprite_loc,Sprite_loc2
        lda Sprite_dir,X
        sta Sprite_dir2,X
.checkleft
        lda #22
        sta SPRT_LOCATOR
        ;; check if we can go right
        jsr changehoriz
        bcs .done
        ;; we could go left
        move16x Sprite_loc2,W1
        jsr CalcDistance

        lda Sprite_offset2,X                  ;restore sprite offset
        sta Sprite_offset,X
        move16xx Sprite_loc,Sprite_loc2
        lda Sprite_dir,X
        sta Sprite_dir2,X
.done
        rts
#if 0        
        ;; calculate W2: the sprite ORG tile
        move16x Sprite_loc,W1   ;
        OrgOffsetByDir          ;figure out org offset
        sta S3                  ;
        subxx W1,W2,S3          ;normalize W2 to upper left 

        lda $ff
        sta S3                  ;initialize least distance to a big number
        sta S4                  ;initialize best move to an invalid move

        ldy #1                  ;check up
        lda (W2),Y
        jsr IsWall
        beq .left
        jsr CalcDistance        ;if result of calc is < S3 then update S3
        UpdateMinDist S3,S4                  ;update least distance score
.left        
        ldy #22
        lda (W2),Y
        jsr IsWall
        beq .right
        jsr CalcDistance
        UpdateMinDist S3,S4
.right        
        ldy #24
        lda (W2),Y
        jsr IsWall
        beq .down
        brk
        jsr CalcDistance
        UpdateMinDist S3,S4
.down        
        ldy #45
        lda (W2),Y
        jsr IsWall
        beq .nomove
        jsr CalcDistance
        UpdateMinDist S3,S4
.nomove
#endif        
        rts
        
GhostAI SUBROUTINE
        txa
        pha
        ;; lda 197
        ;; ldx #0
        ;; jsr DisplayNum
        ;; rts
        ;; caculate pacman row,col so ghosts can use
        ;; in their AI routines
        move16 Sprite_loc,W1
        sub16Im W1,screen
        jsr Divide22_16
        lda W1
        sta PACCOL
        lda S1
        sta PACROW

        ldx #0
        lda PACCOL
        jsr DisplayNum
        ldx #4
        lda PACROW
        jsr DisplayNum

        pla
        tax
        
        rts

;;; W1 contains dividend ( word )
;;; w1 contains remainder on exit
;;; S1 contains result up to 5 bits of precision
;;; this, in the context of the game screen
;;; S1 is the row, and A is the column
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
        

;;; 
;;; Service PACMAN, read joystick and move
;;; 
Pacman SUBROUTINE
        ldx #0
        stx MOVEMADE
        readJoy
        cmp LASTJOYDIR          ;same as last joystick reading
        beq .uselast            ;then use last reading
        and #$bc
        eor #$bc
        beq .uselast
        lda LASTJOY
        jmp .process
.uselast
        ldy MOVEMADE            ;if we made a move already then exit
        beq .cont
        rts
.cont        
        ldy #1
        sty MOVEMADE            ;indicate we made a move
        lda LASTJOYDIR
        sta LASTJOY
        
.process        
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
.down
        store16 PAC1D,W3
        jsr scroll_down
        bcc .moveok
        bcs .uselast
        rts
.right
       store16 PAC1,W3    ;
        scroll_right
        bcc .moveok
        bcs .uselast
        rts
.left
       store16 PAC_L1,W3
        scroll_left
        bcc .moveok
        bcs .uselast
        rts
.up
       store16 PAC_UP1,W3
        jsr scroll_up
        bcc .moveok
        jmp .uselast
        rts
.fire
        rts
.moveok
        lda LASTJOY
        sta LASTJOYDIR
        move16 W3,Sprite_src
#if 1
        jsr Animate
#endif        
.done        
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
        
        ;; load sprite head tile screen position pointer into {2}
        ;; X = sprite
        mac ldSprtHeadPos
        move16x {1},{2}
        endm
        ;; load sprite's tail tile screen position opinter into {2}
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
        endm
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
#if 0        
        mac cmp16
        clc
        lda {1}
        cmp {2}
        bne .done
        lda {1}+1
        cmp {2}+1
        bne .done
        sec
.done        
        endm
#endif        
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
        brk                     ;we just loaded a mazewall, something is wrong
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
        brk                     ;we just loaded a mazewall, something is wrong
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

        ldSprtHeadPos Sprite_loc,W3 ;SPRITE_IDX's current head position into W3
        cpx  SPRT_CUR           ;are we checking against ourselves?
        beq .ourselves          ;ok, W3 is good to go
        ldSprtHeadPos Sprite_loc2,W3 ;not ourselves, load W3 with S3 sprites's new position
.ourselves        
        ;; check if we hit sprite IDX's head with our head
        cmp16 W1,W3
        bne .not_head2head
        jsr head2head
.not_head2head        
        ;; check sprite S3's head for collision with our tail
        cmp16 W2,W3
        bne .not_tail2head
        jsr tail2head
.not_tail2head        
x        ldSprtTailPos Sprite_loc,W3 ;S3 sprite's current tail position into W3
        cpx  SPRT_CUR           ;are we checking against ourselves
        beq .ourselves2         ;ok, W3 is good to go
        ldSprtTailPos Sprite_loc2,W3 ;not ourselves, load W3 with S3 sprites's new position
.ourselves2        
        cmp16 W1,W3
        bne .not_head2tail
        jsr head2tail 
.not_head2tail        
        cmp16 W2,W3
        bne .not_tail2tail
        jsr tail2tail
.not_tail2tail        
        jmp .loop
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
        lda Sprite_sback2,X     ;load tail background
        cmp #NOTCOPY                ;is it still 'unset'
        bne .done               ;no, it's been set already, we are done
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
        ;; ldx SPRT_CUR
        ;; jsr dumpBack2
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

scroll_horiz SUBROUTINE
#if _debug        
        lda #0
        sta S7
#endif        
        lda Sprite_dir,X
        cmp #1
        beq .ok                 ;ok to move horizontal
        ;; switch to horiz order
#if _debug        
        lda #42
        sta S7
#endif        
        jsr changehoriz
        bcc .ok
        rts
.ok
        ldy Sprite_offset,X
        cpy END_SCRL_VAL
        bne .draw
.course                         ;course scroll
#if  _debug        
        lda #42
        cmp S7
        bne .ok1
        brk                     ;shouldn't be course scrolling if we changed orientation
.ok1
#endif        
        move16x Sprite_loc,W2   ;
        lda SCRL_VAL
        bmi .left
        ;; going right
        lda #0
        pha
        inc16 W2
        ldy #01       
        lda #MW
        cmp (W2),Y              ;check for wall at pos + 2
        bne .continue
.cantmove
        sec                     ;can't move, return false
        pla                     ;pop the single arg we pushed for our own use
        rts
.left
        lda #8
        pha
        dec16 W2                ;check to the left for wall
        lda #MW
        ldy #0
        cmp (W2),Y
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
;        store16 PAC_UP1,W1
;        move16x2 W1,Sprite_src
        clc
        rts

;;; change orientation to horizontal
;;; X sprite to attempt change on
;;; SPRT_LOCATOR direction to change to ( 22=left 24=right)
;;; S2 offset from W2 of erase tile
;;; output: sprite_loc2 contains new head position if successful
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
        
        ldy SPRT_LOCATOR
        lda (W2),Y
        cmp #MW                 ;is it a wall
        beq .failed             ;we hit a wall, abort move
        
        lda #1                  ;change direction to horiz
        sta Sprite_dir2,X

        lda #0                  ;assume moving right
        sta Sprite_offset,X     ;sprite offset = 0

        lda #23                 ;offset from ORG for new sprite pos
        sta S3                  ;if moving right

        lda SPRT_LOCATOR        ;moving right?
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
;;; carry is set on return if move is not possible
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
        store16 PAC1D,W1
        clc
        rts
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
;;; BEGIN custom character set
;    org mychars                 ;
;        dc.b $ff
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