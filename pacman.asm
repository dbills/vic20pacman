#ifnconst LARGEMEM              ;if it wasn't specified on the command line
;LARGEMEM equ 1                 ; generate code for 8k expansion
#endif        
INVINCIBLE equ 1                ; pacman can't die
;MASTERDELAY equ 1               ;enable master slowdown for debugging
masterSpeed      equ 250 ;master game delay
PACDEATHGFX equ 1        
;;;
;;; uncomment this to create code that will launch
;;; from basic
;BASIC equ 1    
;;; uncomment to have unlimited lives
;;; altough the game will still only display 3
;UNLIMITED_LIVES equ 1
;;; level that the game starts from normal should be -1
;;; at 2 ghost are as fast as pacman
;;; at 4 ghosts are fast than pacman
STARTLEVEL equ -1
;;; set below to something to run a 'short maze'
;;; that is whatever you set this to, will be the number of dots
;;; you have to eat before the level ends and moves to the next
;SHORTMAZE equ 25
;;; comment this out to not flash the maze at the end of the levels
;;; for faster debugging when running through levels
FLASHMAZE equ 1
;;; maximum number of cherries that can appear on left side
MAXLEVELCHERRIES equ 15
;;; comment out to have fruit that never spoils
AGEFRUIT equ 1
;;; uncomment to prevent ghosts from exiting based on dots eaten count
;;; they will only exit based on timer criteria
;;; 
;NOGHOSTDOTS equ 1
;;; uncomment to show chase/scatter mode debugging at top of screen
;SHOWTIMER1 equ 1
;;; score when a bonus life is given ( the high byte of a 3 byte BCD number )
;;; e.g. 10000 points is 010000 or $01
BONUSLIFE equ $01
;;; if uncommented, play the intro music
;ACTORINTRO equ 1                ;
;;;
;;; uncomment to activate ghosts
;;; comment out to cause no ghost movement
GHOSTS_ON   equ 1    ;
;;;
;;; uncomment to see the matrix
;;; mode, that is no video chars
;MATRIX equ 1
;;;
;;; allow the ghost to be moved with
;;; keyboard commands
;;; 
;GHPLAYER    equ 1              ; ghost as player
;;;
;;; uncomment to penalize pacman as well as ghosts
;;; when going through the tunnel
;;;
;PACTUNNEL equ 1        
;;;
;;; PACMAN 2014 ( hopefully )
;;; VIC20 6502 versions for +3k and +8k machines
;;; title name: Panicman
;;; 
#ifconst LARGEMEM
        org $1201
#else        
        org $0401
#endif        
        processor 6502
#ifconst BASIC

;;; inject code for a BASIC 'sys' command
        HEX 0c 04 0a 00  9e 20
#ifconst LARGEMEM
        dc.b  "4","6","2","2"
#else        
        dc.b  "1","0","3","8"
#endif        
        HEX 00 00 00
        jmp main
#else                           ;not basic startup
        jmp main

#endif // BASIC
        
;;; chase table is the initial scatter/chase phases for each level
;;; they change as levels go on
;;; 7 seconds, 20 seconds, etc...
;;; even values are scatter mode, odd are chase mode
;;; iteration starts from end 
ChaseTable     dc.w  (5*60)*softTimerRes, 5*softTimerRes, 20*softTimerRes, 7*softTimerRes, 20*softTimerRes, 7*softTimerRes
ChaseTableEnd
ChaseTableSz  equ [[ChaseTableEnd-ChaseTable]/2] ;entries in above table 
;;; 
;;; division table for division by 22
Div22Table_i      dc.w [22*1],[22*2],[22*4],[22*8],[22*16]
;;; the home tiles for ghosts that are in 'scatter' mode
;;; ghosts try to find their way to these home tiles
GhosthomeTable  dc.b inkyHomeCol,inkyHomeRow,blinkyHomeCol,blinkyHomeRow,pinkyHomeCol,pinkyHomeRow,clydeHomeCol,clydeHomeRow
VolTable        dc.b 1,2,3,4,5,6,7,8,7,6,5,4,3,2,1,0
VolTableSz equ 15        
;;; eating dots sound, belong in audio.asm 
WakaTable      
        dc.b 236
        dc.b 239
        dc.b 242
        dc.b 245
        dc.b 247
        dc.b 248
        dc.b 0
        dc.b 0
        dc.b 0
        dc.b 248
        dc.b 247
        dc.b 245
        dc.b 242
        dc.b 239
        dc.b 236
        dc.b 0
        dc.b 0
        dc.b 0
WakaTableEnd
;;; groups of 7 bytes
;;; Pacman speed: normal, dots, power, pwrdot
;;; Ghost speed:  normal, frightened, tunnel
Lvl1Spds
        dc.b 40,58,20,42        ;lvl 1
        dc.b 50,100,120
;        dc.b 170,100,120
Lvl2Spds        
        dc.b 20,42,10,37
        dc.b 30,90,110
Lvl5Spds
        dc.b 0,26,0,26
        dc.b 10,80,100
Lvl21Spds        
        dc.b 20,42,0,26
        dc.b 10,80,100
    
#ifconst LARGEMEM
        org $1400-(8*3)
        INCLUDE "bitmaps.asm"
        org $1400+$800          ;full 2K character set
#endif        

sirenBot    equ 227
sirenTop    equ 238
speedBase   equ 201               ; see speed calculations at EOF
;_SLOWPAC       equ 1             ; pacman doesn't have continuous motion
LARGEMAZE   equ 1                 ;
_debug      equ 1                 ; true for debugging
cornerAdv   equ 1                 ; pacman's cornering advantage in pixels
wakavoice   equ 36874        
voice1      equ 36874             ; sound registers
voice2      equ 36875
voice3      equ 36876
voice4      equ 36877        
volume      equ 36878
        
#ifconst LARGEMEM
        
screen      equ $1000             ; screen ram
clrram      equ $9400             ; color ram for screen
clroffset   equ $84               ; offset from screen to color
mychars     equ $1400             ; >=8K font start
        
#else
        
clroffset   equ $78               ; offset from screen to color
screen      equ $1e00             ; 3k ram
clrram      equ $9600             ; color ram for screen (3k)
mychars     equ $1c00             ; 3K font start
        
#endif // LARGEMEM
        
#ifconst GHPLAYER        
defaultISR  equ $eabf             ; os default IRQ
#else        
defaultISR  equ  $eb15            ; the minimum isr ( no keyboard polling or other stuff )
#endif        
defaultVol  equ 8                 ; default volume for app
VICRASTER   equ $9004        
VICSCRN     equ $9005             ; vic chip character generator pointer
LIGHPENX    equ $9006             ; used for random number
VIA1DDR     equ $9113
VIA2DDR     equ $9122             ; ?
JOY0        equ $9111
JOY0B       equ $9120             ; output register B VIA #2
JOYUP       equ $4                ; joy up bit
JOYDWN      equ $8                ; joy down
JOYL        equ $10               ; joy left
JOYT        equ $20               ; joy fire
JOYR        equ $80               ; joy right bit
;;; 00000000
;;;   TLDU   
;;; 00111100
;;; 10111100 = bc
;;; 10011100 = 9c
VALIDMOVE equ $9c
chrom1      equ $8000             ; upper case character rom
chrom2      equ $8400             ; upper case reversed
chrom3      equ $8c00             ; upper lower reversed
chrom4      equ $8800             ; upper lower
;;; 
;;; I do store some pointers in the casette buffer
;;; to save memory in the 3K version
;;; 
cassStart   equ $033d             ;start of cassette buffer ( 190 bytes)
cassEnd     equ $03fb
SaveBuffer  equ cassEnd-20

;; vic-I chip registers
chrst           equ $9003       ; font map pointer
charcnt         equ $800
zeroDigit       equ 48 | $80    ;zero digit character
motionRight     equ 24          ;sprite locator code for right
motionLeft      equ 22          ;sprite locator code for left
motionUp        equ 1           ;sprite location code for up
motionDown      equ 45          ;sprite location for down
tunnelRow       equ 11          ;row number that tunnel lives on
tunnelRCol      equ 20          ;column to start warp to left side
tunnelLCol      equ 1           ;column to start warp to right side
;;; the speed warp effect would start at tunnelLCol+tunnelLen
;;; or tunnelRCol-tunnelLen
tunnelLen       equ 3           ;length of tunnel
;;; amount of time a fruit is display
fruitTime       equ 140
        
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
;;; S or byte, short for 'scratch' and are
;;; usually bytes
W1              equ 0
W2              equ W1+2
W3              equ W2+2       
W4              equ W3+2       
S0              equ W4+2     
S1              equ S0+1       
S2              equ S1+1       
S3              equ S2+1
S4              equ S3+1
DIV22_WORK      equ S4+1                  ;word
DIV22_RSLT      equ DIV22_WORK+2          ;div22 result
SPRITEIDX       equ DIV22_RSLT+1      ; sprite index for main loop
MASTERCNT       equ SPRITEIDX+1       ; countdown; see masterDelay
;;; 
CSPRTFRM        equ MASTERCNT+1       ; number of frames in the currently processing sprite
DOTCOUNT        equ CSPRTFRM+1        ; dots eaten
frameCount      equ 4                 ; number of pacman animation frames
PACFRAMED       equ DOTCOUNT+1        ; pacframe dir
DSPL_1          equ PACFRAMED+1       ; used by DisplayNum routine
BCD             equ DSPL_1+1          ; used by Bin2Hex routine
DSPL_2          equ BCD+1       
DSPL_3          equ DSPL_2+1        
CHASEMODE       equ DSPL_3+1          ; ghosts in scatter mode or chase
GHOST_DIST      equ CHASEMODE+1       ; best distance for current ghost AI calcs
GHOST_DIR       equ GHOST_DIST+1      ; best move matching GHOST_DIST
DIV22_REM       equ GHOST_DIR+1        
PACCOL          equ DIV22_REM+1       ; current pacman column
PACROW          equ PACCOL+1          ; current pacman row
;;; sprite position used by AI from most recent call to any of the
;;; directional changing routines ( up,left etc )
GHOST_TGTCOL    equ PACROW+1
GHOST_TGTROW    equ GHOST_TGTCOL+1
GHOST1_TGTCOL   equ GHOST_TGTROW+1    ; blinkys target tile? calculated from pinky's routine
GHOST1_TGTROW   equ GHOST1_TGTCOL+1
;;; amount sprite move routines can shave off during cornering
;;; pacman get +1 on corners, ghosts get 0
CORNER_SHAVE    equ GHOST1_TGTROW+1
;;; non zero when pacman is powred up, indicate 'gameloop units' 
;;; left in power mode, this does not use the jiffy timer
POWER_UP        equ CORNER_SHAVE+1
basePowerTime   equ 245               ;initial power pill time
BlinkyS1        equ POWER_UP+1         
BlinkyS2        equ BlinkyS1+1
InkyDots        equ BlinkyS2+1
PinkyDots       equ InkyDots+1
ClydeDots       equ PinkyDots+1
S5              equ ClydeDots+1   
S6              equ S5+1
PACXPIXEL       equ S6+1
PACYPIXEL       equ PACXPIXEL+1       
BlinkyCruise    equ PACYPIXEL+1       ;see BlinkyCruise1 and 2 

GHOST_COL       equ BlinkyCruise+1
GHOST_ROW       equ GHOST_COL+1
W5              equ GHOST_ROW+1
W5_h            equ W5+1
W6              equ W5_h+1
W6_h            equ W6+1    
PowerPillTime   equ W6_h+1
;;;offset for normalizing a 9x9 sprite movement block to the upper left block
;;; used by the sprite orientation changing routines
SPRT_LOCATOR    equ PowerPillTime+1
;value that indicates end of smooth scrolling
END_SCRL_VAL    equ SPRT_LOCATOR+1
;;; amount to increase or decrease sprite offset
;;; used as input by ScrollHoriz
SCRL_VAL        equ END_SCRL_VAL+1
LASTJOY         equ SCRL_VAL+1
LASTJOYDIR      equ LASTJOY+1         ;last joy reading that had a switch thrown
MOVEMADE        equ LASTJOYDIR+1      ;true if last pacman move was successful
TIMER1          equ MOVEMADE+1        ;compared against jiffy clock for chase/scatter modes
TIMER1_h        equ TIMER1+1          ;timer1 high byte
TIMER1_hh       equ TIMER1_h+1        
r_seed          equ TIMER1_hh+1       ; random number seed
Div22Table      equ r_seed+1          ; table for fast 22 division
;;; ----------- 10 bytes
Sprite_offset   equ Div22Table+10
;;; ------------5 bytes
PacLives        equ Sprite_offset+5
SirenIdx        equ PacLives+1
EatenIdx        equ SirenIdx+1 ; number of ghosts eaten since power pill
PACDEATH        equ EatenIdx+1 ; pacman death animation pointer
ChaseTableIdx   equ PACDEATH+1
PowerPillPtr    equ ChaseTableIdx+1        ;ptr to power pill sound
Audio1          equ PowerPillPtr+1         ;see audio.asm
FruitSoundOn    equ Audio1+1
FruitPillPtr    equ FruitSoundOn+1
FruitIsOut      equ FruitPillPtr+1         ;true if fruit is displayed
DeathSoundPtr   equ FruitIsOut+1           ;sound table index
         
JIFFYH          equ DeathSoundPtr+1        ; jiffy clock lsb - msb
JIFFYM          equ JIFFYH+1
JIFFYL          equ JIFFYM+1
SAVE_OFFSET     equ JIFFYL+1
SAVE_OFFSET2    equ SAVE_OFFSET+1
SAVE_DIR        equ SAVE_OFFSET2+1
SAVE_DIR2       equ SAVE_DIR+1
Sprite_src      equ SAVE_DIR2+1           ;10 bytes
Sprite_motion   equ Sprite_src+10
Sprite_turn     equ Sprite_motion+5       ;5 bytes
SirenDir        equ Sprite_turn+5         ; siren scale direction
;;; signals to stop the waka sound after completion of next cycle
;;; see SoundOn. 0 = halt/halted 1 = do not halt/not halted
;;; a waka sound must complete it's full 'cycle', so you can't
;;; just hear "half a waka".
;;; waka sound is turned off in the following conditions:
;;; 1) during wall checks ( which is weird )
eat_halt        equ SirenDir+1
;;; signals that the waka sound is fully stopped
eat_halted      equ eat_halt+1
WakaIdx         equ eat_halted+1
flashRate       equ 14              ;in 60s second
PwrFlashCnt     equ WakaIdx+1       ;countdown to flash power pill
PwrFlashSt      equ PwrFlashCnt+1   ;state of power pill flash 0 = blank
bonusInterval   equ 7               ;sound interval for award noise
BonusAwarded    equ PwrFlashSt+1    ;true if bonus life was awarded
Agonizer        equ BonusAwarded+1  ;keeps track of when to increment difficulty
BonusSound      equ Agonizer+1
NewOffset       equ BonusSound+1    ; used by ScrollHoriz routine
PrevSprtMotion  equ NewOffset+1
SirenTable      equ PrevSprtMotion+1
SirenOffset     equ SirenTable+((sirenTop-sirenBot)*2)+1
PowerSnd2Idx    equ SirenOffset+1
FrameLock       equ PowerSnd2Idx+1
Sprite_page     equ FrameLock+1
Xsave           equ Sprite_page+1    
Sprite_loc      equ $a7
Sprite_loc2     equ Sprite_loc+10    ;new screen loc
Sprite_back     equ Sprite_loc2+10   ;background char before other sprites drawn
Sprite_back2    equ Sprite_back+5    ;static screen background
;;;current screen background ( might include some other sprite tile that was laid down )
Sprite_sback    equ Sprite_back2+5 
Sprite_sback2   equ Sprite_sback+5
Sprite_dir      equ Sprite_sback2+5  ;
Sprite_dir2     equ Sprite_dir+5     ;sprite direction 1(horiz),22(vert)    
;Sprite_offset   equ Sprite_dir2+5   ;sprite bit offset in tiles
Sprite_offset2  equ Sprite_dir2+5    ;upcoming sprite bit offset in tiles
PlayerScore_h   equ Sprite_offset2+5 ;3 byte BCD player score, MSB order
PlayerScore_m   equ PlayerScore_h+1
PlayerScore_l   equ PlayerScore_m+1
ResetPoint      equ PlayerScore_l+1           ;stack reset location for game reset ( longjmp )
LevelsComplete  equ ResetPoint+1              ;length 1 : number of levels completed so far
Sprite_mode     equ LevelsComplete+1          ;length 5
;;; your turn gets skipped every N loops of Sprite_speed
;;; for example: if sprite speed for sprite 0 =10
;;; then every 10th game loop that sprite doens't get to move
;;; see equates for Sprite_fast, Sprite_standard, Sprite_slow
;;; for basic speeds
Sprite_speed    equ Sprite_mode+5  ; current sprite speed 
Sprite_base     equ Sprite_speed+5 ; base speed of sprites for this level
Sprite_frame    equ Sprite_base+5  ; animation frame of sprite as offset from _src
;;; points for eating fruits
fruitPoints     equ Sprite_frame+5
    
CURKEY          equ $c5             ;OpSys current key pressed
;;; sentinal character, used in tile background routine
;;; to indicate tile background hasn't been copied into _sback yet
NOTCOPY         equ $fd
;;; used by the AI engine to indicate the worst possible choice
noChoice        equ $7f

;;
;; misc constants for graphical tiles
;;
CHERRY          equ [BIT_CHERRY-CHAR_BEGIN]/8 
EMPTY           equ [BIT_EMPTY-CHAR_BEGIN]/8
PWR2            equ [BIT_PWR0-CHAR_BEGIN]/8
PWR             equ [BIT_PWR1-CHAR_BEGIN]/8
RTOP            equ [BIT_RTOP-CHAR_BEGIN]/8   ;right top corner
DOT             equ [BIT_DOT-CHAR_BEGIN]/8
HWALL           equ [BIT_HWALL-CHAR_BEGIN]/8
VWALL           equ [BIT_VWALL-CHAR_BEGIN]/8
;;; character we use for pacman life indicator
PACLIFE         equ [BIT_PACRIGHTOPEN-CHAR_BEGIN]/8
GHOST_WALL      equ [BIT_GHWALL-CHAR_BEGIN]/8        
GHL             equ [GHOST_BEGIN-CHAR_BEGIN]/8
GH1L            equ [GHL+4]
GH2L            equ [GH1L+4]
GH3L            equ [GH2L+4]    
PACL            equ [GH3L+4] 
;
;------------------------------------
; Utility macros
;------------------------------------
                                ;
        INCLUDE "macros16.asm"
        ;; logical not of 1, used to switch between on/off states
        MAC Invert
        lda #1                  ;dbl buffering, switch sprite tiles
        eor {1}                 ; 0 = 1
        sta {1}                 ; or 1 = 0
        ENDM
        MAC saveY
        tya
        pha
        ENDM
        MAC resY
        pla
        tay
        ENDM
        ;; save X
        MAC saveX
        stx Xsave
        ENDM
        ;; restore X
        MAC resX
        ldx Xsave
        ENDM
        MAC saveAll
        pha
        txa
        pha
        tya
        pha
        ENDM
        MAC resAll
        pla
        tay
        pla
        tax
        pla
        ENDM
        ;; test if jiffy timer > timer1
        MAC HasTimerExpired
           sei         ;disable interrupt while we compare against the clock
           LDA JIFFYH  ; compare high bytes
           CMP {1}+2
           BCC .LABEL2 ; if JIFFYH < NUM2H then NUM1 < NUM2
           BNE .LABEL1 ; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)
           LDA JIFFYM  ; compare middle bytes
           CMP {1}+1
           BCC .LABEL2 ; if NUM1M < NUM2M then NUM1 < NUM2
           BNE .LABEL1 ; if NUM1M <> NUM2M then NUM1 > NUM2 (so NUM1 >= NUM2)
           LDA JIFFYL  ; compare low bytes
           CMP {1}
           BCC .LABEL2 ; if NUM1L < NUM2L then NUM1 < NUM2
.LABEL1                ; notify timer1 expired
           cli
           jsr {2}
.LABEL2
           cli
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
;;; initialize the sprite loop counter
;;; kinda a lame macro I guess
;;; but I thought the name helped readability
        MAC InitSpriteLoop
        lda #SPRITES
        sta SPRITEIDX
        ENDM
        ;; jump to game reset point
        ;; mode={1}
        MAC JmpReset
        lda #{1}
        sta S1
        jsr longJmp
        ENDM
        ;; 
        ;; display 16 bit debugging
        ;; info on screen
        MAC Display2
        pha                     ;save A
        txa
        pha
        
        lda #[{1}-"A"+1 | $80]
        ldx #{2}
        sta screen,X

        lda {3}
        sta BCD
        jsr DisplayBCD

        inx
        lda {4}
        sta BCD
        jsr DisplayBCD


        pla
        tax

        pla                     ;restore A
        ENDM
;;; display in hex
;;; display a single byte {3} at offset {2} on top line prefixed by char {1}
        MAC Display1
        saveAll
        
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
        resAll

        ENDM
        ;; wait for number jiffies in {1}
WaitTime_ subroutine
        clc
        adc JIFFYL
.gettime        
        cmp JIFFYL
        bne .gettime
        rts
        ;; wait for number seconds in {1}
        ;; no more than 255/60 seconds possible
        MAC WaitTime
        lda #60*{1}               ;seconds
        jsr WaitTime_
        ENDM
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
        ;; duh, you can't use beq with this
        MAC dec16
        lda  [{1}]+0
        bne .done
        dec [{1}]+1
.done
        dec [{1}]+0
        ENDM

;;; find the character font address of the tile
;;; underneath a sprite
;;; on entry: A = tile in question
;;; {1} (out) font address of tile underneath
;;; uses S5,S6,W5
        MAC mergeTile
        ldy #0
        sty W5
        sty W5+1
;        clc
        asl 
        rol W5+1
        asl
        rol W5+1
        asl
        rol W5+1

        clc
        adc #mychars&$ff
        sta {1}
        lda W5+1
        adc #mychars >> 8
        sta {1}+1

        ENDM
;; allows beq on joy right A has bit 7 of last reading from JOY0B
        MAC onjoyr
        lda JOY0B               
        and #JOYR
        ENDM
;;; read joystick value into LASTJOY
        MAC readJoy
        
        lda JOY0
        and #$7f                ;clear bit 7 ( joy right )
        sta LASTJOY
        onjoyr                  ;read joystick right pos
        ora LASTJOY             ;or in bit 7 from that read
        sta LASTJOY             ;store it
.done        
        ENDM

        INCLUDE "debug.asm"

;;; we 'page flip' the tiles so that the expensive bit rendering
;;; does not have to be done on the vertical blank as my routines
;;; sprite_page controls which set of tiles we draw , it alternates
;;; between 0 and 1
;;;
pacframes       equ #3            ; total number of pacman animation frames ( 1 based )
dirVert         equ 22            ;sprite oriented vertically
dirHoriz        equ 1             ;sprite oriented horizontally
;;; valid modes for JmpReset
modePacDeath    equ 0             ;see JmpReset
modeEndLevel    equ 1             ;see JmpReset
modeResetGame   equ 2             ;see JmpReset
;;; valid modes for Sprite_mode
modeInBox       equ 0
modeFright      equ 1
modeEaten       equ 2             ;ghost was chomped
modeLeaving     equ 3             ;leavin the ghost box
modeOutOfBox    equ 4             ;see sprite_mode
modePacman      equ 5             ;mode only pacman has
;;; causes ghosts to reverse direction
;;; changes from scatter to chase cause reverse for example
modeReverse     equ 6
modeFright0     equ 7        
;;; end valid modes for Sprite_mode
msgRow          equ 13            ;row number for displaying messages
cherryCol       equ 21/2+1        ;to display bonus fruit
cherryRow       equ msgRow        ;to display bonus fruit
outOfBoxCol     equ 11            ;column at ghost box entry/exit
outOfBoxRow     equ 9             ;row of tile directly above exit
pacStartRow     equ outOfBoxRow+8 ;pacman start row
pacStartCol     equ outOfBoxCol        
pacStart        equ screen+22*pacStartRow+pacStartCol
testDot         equ pacStart+4        
pinkyHomeCol    equ 3        
pinkyHomeRow    equ 3
blinkyHomeCol   equ 22-3
blinkyHomeRow   equ 3
clydeHomeCol    equ 6
clydeHomeRow    equ 23-2
inkyHomeCol     equ 23-6
inkyHomeRow     equ 23-2        
;;; screen location of ghost box exit, the block above it
ghostBoxExit    equ [screen+[22*outOfBoxRow]+outOfBoxCol]
ghostBoxHall    equ ghostBoxExit+22
leftMargin      equ 1           ;wasted space on left of screen
;;; the ghost's by X register offset
inky            equ 1        
blinky          equ 2
pinky           equ 3
clyde           equ 4
nobody          equ 10        
focusGhost      equ nobody       ;ghost to print debugging for
totalDots       equ $A2+4        ;total dots in maze
;totalDots       equ 10          ;total dots in maze
fruit1Dots      equ 70           ;dots to release fruit
fruit2Dots      equ 120          ;dots to release fruit2
clydeDots       equ totalDots-30 ;dots to release clyde ( about 33% )
inkyDots        equ totalDots-10 ;dots to release inky  ( )
pinkyDots       equ totalDots-20 ;dots to release pinky ( should be 1)


maxLives        equ 4           ;max lives ever possible on left display
pacLives        equ 3           ;default starting lives for pacman
#ifconst LARGEMAZE
g1Start         equ screen+22*11+9
g2StartI        equ 22*outOfBoxRow+outOfBoxCol
g2Start         equ screen+g2StartI
;;; debug location, buttom row
;g2Start         equ screen+22*21+2
g3Start         equ screen+22*11+10
g4Start         equ screen+22*11+12
#else
g1Start         equ screen+22*11+10
g2Start         equ screen+22*outOfBoxRow+outOfBoxCol
g3Start         equ screen+22*11+11
g4Start         equ screen+22*11+11
#endif        
;;; saved tunnel speed when ghosts are in tunnel

Sprite_tile     dc.b PACL,GHL,GH1L,GH2L,GH3L      ;foreground char
;;; sprite chargen ram ( where to put the source bmap )
Sprite_bmap     dc.w mychars+(PACL*8),      mychars+(GHL*8)      ,mychars+(GH1L*8)     , mychars+(GH2L*8)     , mychars+(GH3L*8)    
Sprite_bmap2    dc.w mychars+(PACL*8)+(2*8),mychars+(GHL*8)+(16) ,mychars+(GH1L*8)+(16), mychars+(GH2L*8)+(16),mychars+(GH3L*8)+(16)
;;; table of sprite offset for ghosts in box
inBoxTable      dc.b 0,0,0,2,6
;;; for eating ghosts 200,400,800,1600 in bcd
pointTable      dc.b $16,00,$08,00,$04,00,$02,00

;;; the current sprite speeds
;;; speeds when pacman is powered up
eyeSpeed         equ 255                ;sprite_speed setting for eyes
pacEatSpeed      equ 255
ghostFrightSpeed equ speedBase*2
;;; the speed of ghosts when pacman is powered up
Sprite_speed2    dc.b pacEatSpeed,ghostFrightSpeed,ghostFrightSpeed,ghostFrightSpeed,ghostFrightSpeed
;;; see speed tables at the end of this file
;;; values are in X/200ths
Speed_standard   equ 80         ;85%
Speed_slow       equ 40         ;80%
Speed_fast       equ 20         ;100%
harder1          equ 2          ;level when ghost are speed standard
harder2          equ 4          ;level when ghosts are faster
Sprite_turnbase  dc.b 200,200,200,200,200
Sprite_color     dc.b #YELLOW,#CYAN,#RED,#GREEN,#PURPLE
;;; cruise elroy setting for blinky
blinkyCruise1    equ 1
blinkyCruise2    equ 2
blinkyCruiseOff  equ 0
;;; 
;;; resolution of system timer in 1/x seconds
softTimerRes   equ 60
;;; swap upcoming sprite position data with current sprite data
;;; i.e. page flip the screen location
        MAC SwapSpritePos
        
        move16xx Sprite_loc2,Sprite_loc
        lda Sprite_dir2,X
        sta Sprite_dir,X
        lda Sprite_offset2,X
        sta Sprite_offset,X

        ENDM
;;; horizontal blit
;;; W1 = source bits
;;; W2 = left tile dest bits
;;; W3 = right tile dest bits
;;; W4 = under character bitmap
;;; S1 = amount to shift ( 1 - 8 )
;;; S2 = sprite to move
        MAC blith 
#if 1
        ldx S2

        lda Sprite_sback,X      ;input to mergeTile
        mergeTile W6              ;font address of underneath tile into W4

        lda Sprite_sback2,X
        mergeTile W4
#endif        
        ldy #7
        ldx #0
.nextbyte
        stx S4
        lda (W1),Y              ;load sprite source byte
        sta S3
        ldx S1                  ;load amount to scroll to right
        beq .shiftdone          ;done scrolling?
.loop
        lsr S3                    ;scroll head tile
        ror S4                    ;scroll it
        dex
        bne .loop
.shiftdone
        lda (W6),Y              ;load left tile underneath bits
        ora S3
        sta (W2),Y
        lda (W4),Y              ;load right tile underneath bits
        ora S4
        sta (W3),Y
        dey
        bpl .nextbyte
.done
        ENDM
;;; vertical blit
;;; W1 = source bits
;;; W2 = left tile dest bits
;;; W3 = right tile dest bits
;;; S1 = amount to shift ( 1 - 8 )
;;; S2 = sprite to move
;;; uses W4,S5,S6
;;; 
        MAC blitd 
        ;; get left hand tile 'underneath' bitmap
        ;; so we can or it into this new image
        ldx S2                 
        lda Sprite_sback,X      ;input to mergeTile

        mergeTile W4            ;font address of underneath tile into W4

        ldx #0
        ldy #0
.loop        
        lda (W4),Y              ;load background tile byte
        cpx S1                  ;are we ready to start copying source bitmap
        bmi .lt                 ;nope, skip
        ora (W1),Y              ;yes, or in the source byte
        inc16 W1                ;increment the source byte count
.lt           
        sta (W2),Y              ;store the possibly combined background/sprite byte
        inc16 W4                ;increment background byte pointer
        inc16 W2                ;increment left tile destination byte pointer
        inx                     ;next x
        cpx #8
        bmi .loop

        ldx S2
        lda Sprite_sback2,X

        mergeTile W4

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
        ENDM
;;; S2 sprite to render
render_sprite SUBROUTINE
        stx S2                  ;set for call to blith
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
        
        lda Sprite_page         ;
        beq .page0
        move16x Sprite_bmap,W2  ;left tile chargen ram
        jmp .cont
.page0
        move16x Sprite_bmap2,W2  ;left tile chargen ram
.cont        
        addxx W2,W3,#$8          ;right tile chargen ram
        
        lda Sprite_offset2,X
        sta S1                  ;setup for blitd,blith,blitc
        lda Sprite_dir2,X
        cmp #dirHoriz
        beq .horiz
        cmp #dirVert
        beq .vert
        brk
.horiz        
;        blitc
        blith
        rts
.vert
        blitd
        rts
;;; perform end level animation
;;; S1=color
FlashScreen subroutine
        store16 clrram,W1
        ldy #0
.0
        lda S1
        sta (W1),Y
        inc16 W1
        cmp16Im W1,clrram+22*23
        beq .done
        jmp .0
.done        
        rts
;;; 
;;; Flash the screen at between blue and white
;;; 
FlashMaze subroutine
        jsr AllSoundOff
        lda #90
        jsr WaitTime_
        ldx #0
.0        
        lda #WHITE
        sta S1
        jsr FlashScreen
        lda #30
        jsr WaitTime_
        cpx #2
        beq .done
        lda #BLUE
        sta S1
        jsr FlashScreen
        lda #25
        jsr WaitTime_
        inx
        jmp .0
.done
        WaitTime 2
        rts
;;; figure out what pacman might be eating
;;; A = consumed playfield tile
CheckFood subroutine
        cmp  #CHERRY
        beq .cherry
        
        cmp #PWR
        beq .power_pill

        cmp #DOT
        bne .done
#ifnconst NOGHOSTDOTS
        jsr DotEaten
#endif
.checkdots        
        ;; handle eating dots
        ;; and figuring out if the level is over
        dec DOTCOUNT
#ifconst SHORTMAZE
        lda #[totalDots-SHORTMAZE]
        cmp DOTCOUNT
#endif
        beq .end_level
.done        
        rts
.end_level
#ifconst FLASHMAZE        
        jsr FlashMaze
#endif        
        JmpReset modeEndLevel
        ;; control never reaches here
.cherry ;cherry has been eaten
        ldx #0
        lda fruitPoints,X
        inx
        ldy fruitPoints,X
        jsr UpdateScore
        lda #1
        jmp isr5_reset          ;activate fruit sound player, rts for us
.power_pill        
        jsr PowerPillOn         ;rts for us
        jmp .checkdots

;;; X = sprite to erase
;;; if we are erasing pacman then we need to check if he just ate something
;;; 
erasesprt SUBROUTINE
        cpx #0
        bne .notpac
;        Display1 "O",3,Sprite_offset
        lda #4                  ;were we halfway through a tile
        cmp Sprite_offset
        bne .notpac             ;nope, skip food checks
        ;; what did pacman just eat?
        lda Sprite_back
        jsr CheckFood
        lda Sprite_back2
        jsr CheckFood

        lda #EMPTY
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
        jmp UpdateColorRam      ;rts for us
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

        lda Sprite_mode,X
        cmp #modeFright
        bne .done
        ;; frightened ghosts are blue and flash
        ;; as time runs out of power charge
        lda POWER_UP
        beq .done            ;not in power up mode
        ;; flash ghost color when power up is low
        cmp #20                 
        bcc .near_end
        ldy #BLUE
        bne .done               ;jmp .done
.near_end                       ;almost out of power time
        ldy #WHITE              ;show white
        lda #4                  ;but flash blue when 
        bit POWER_UP            ;power_up modulo 4 is 0
        beq .done
        ldy #BLUE
.done
        ENDM
;;; Y =  byte of color
;;; W1 = screen position of sprite
;;; X = sprite we are working with
;;; this routine needs work, it doesn't correctly
;;; limit the color ram to the minimum cell area
UpdateColorRam SUBROUTINE
        lda #clroffset          ;W1 now = color ram location
        clc
        adc W1+1
        sta W1+1
        tya                     ;color to A
        ;; below checks if we need to set the color in the
        ;; head, tail or both head and tail 
        ldy Sprite_offset,X     ;check if we need to update
        beq .inheadonly
        cpy #8                  ;color in head
        beq .intailonly         ;sprite lives in tail only
        ;; else both need updated
        ldy #0
        sta (W1),Y              ;write head tile color ram
        ldy Sprite_dir,X        ;write tail tile color ram 
        sta (W1),Y
        rts
.inheadonly        
        ldy #0
        sta (W1),Y              ;write head tile color ram
        rts
.intailonly        
        ldy Sprite_dir,X        ;write tail tile color ram 
        sta (W1),Y
        rts
;;; Draw Sprite Tiles To Screen ram
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

        jmp UpdateColorRam      ;rts for us


;;; load reverse direction into A
ReverseDirection subroutine
;        lda #modeOutOfBox
;        sta Sprite_mode,X
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
        rts
.right
        lda #motionLeft
        rts
.up
        ;; careful not to reverse ghosts that are coming
        ;; out of the box
        move16x Sprite_loc,W1
        cmp16Im W1,ghostBoxExit
        beq .down               ;put it back to up
        lda #motionDown
        rts
.down
        lda #motionUp
        rts
        
ReverseGhosts SUBROUTINE
        ldx #5
.loop
        dex
        beq .done
        lda Sprite_mode,X
        cmp #modeOutOfBox
        bne .loop
        lda #modeReverse
        sta Sprite_mode,X
        bne .loop               ;jmp .loop
.done        
        rts
;;; 
;;; manage switching ghosts from chase to scatter modes
;;; 
Timer1Expired SUBROUTINE
        jsr ReverseGhosts
        ldy ChaseTableIdx       ;load current table entry
        ;;
        cpy #5
        bne .next
        ldx #pinky
        jsr LeaveBox
        ldx #clyde
        jsr LeaveBox
.next    
        dey                     ;move down to nxt table entry
        ;;
        ;; TODO
        ;; if ghosts havn't all come out by the beginning of the third
        ;; scatter period, then send them out
        ;;
        bpl .0                  ;branch positive
        ;; < 0 wrapping around table
initChaseTimer        
        ldy #ChaseTableSz-1        ;reload table index to top of table
.0
        sty ChaseTableIdx
        move16y ChaseTable,TIMER1   ;chase time to TIMER1
        sei                         ;prevent clock updates while we do some math
        lda JIFFYL
        clc
        adc TIMER1
        sta TIMER1
        lda JIFFYM
        adc TIMER1+1
        sta TIMER1+1
        lda JIFFYH
        adc #0
        sta TIMER1_hh
        cli                     ;re-enable interrupts
#ifconst SHOWTIMER1        
        Display1 "S",10,#1
#endif
        
        iny                     ;add 1 such that even/odd works the way we want
        tya                     ;
        and #1                  ;bit 0 controls chase or scatter even or odd
        sta CHASEMODE           ;engage chase mode, scatter when clear
        
#ifconst SHOWTIMER1        
        beq .done
        Display1 "C",10,#1
#endif
        
.done
        rts
;;; 
;;; called when a power pill is de-activated
;;; 
PowerPillOff SUBROUTINE
        lda #0
        sta 36876
        sta POWER_UP
        ldy #SPRITES-1
.loop        
        lda Sprite_base,Y
        sta Sprite_speed,Y
        lda Sprite_mode,Y
        cmp #modeFright
        bne .0
        lda #modeOutOfBox
        sta Sprite_mode,Y
.0        
        dey
        bpl .loop
.done        
        rts
;;; 
;;; called when a power pill is activated
;;; 
PowerPillOn SUBROUTINE
        ;; init  sound effect table for isr4
        jsr isr4_reset

        lda #$50                ;bcd '50' points
        ldy #$0                 ;bcd high byte
        jsr UpdateScore
        lda #7                  ;[4*2]-1 4 ghost, 2 bytes
        sta EatenIdx            ;initial ghost bonus points
        lda PowerPillTime       ;init power pill time
        sta POWER_UP            ;store in timer
        ldx #SPRITES-1          ;init loop counter
        ;; install new speed for ghosts that are out of box
.loop
        lda Sprite_mode,X    
        cmp #modeOutOfBox
        bne .0                  ;nope, skip
        ;; install sad ghost bitmaps to sprite
        store16x BLUE_GHOST,Sprite_src
        lda Sprite_speed2,X     ;value from powerpill speed table
        jsr SetSpeed
        ;; ghosts in the maze are now frightened
        lda #modeFright0
        sta Sprite_mode,X
.0        
        dex
        bne .loop               ;break out of loop on pacman
        lda Sprite_speed2       ; load pacman speed
        jmp SetSpeed            ; rts for us
;        rts
;;; 
;;; run the siren soundtrack
;;; siren changes pitch when blinky gets faster
;;; 
isr3 subroutine
        ldy SirenIdx
        lda SirenTable,Y
        beq .reset
        clc
        adc SirenOffset
        sta 36876
        inc SirenIdx
        rts
.reset
        sta SirenIdx
        rts
        
;;; power pill sound table
PowerPillTable dc.b 227,232,236,239,241,247,0 ;null terminated
;;; macro to generate a table based sound player
;;; {1} the table index
;;; {2} the voice
;;; {3} the table base
        MAC TableSoundPlayer
        ldy {1}
        lda {3},Y
        beq {4}
        sta {2}
        inc {1}
        rts
{4}
        sta {1}        ;reset index to 0
        rts
        ENDM
;;; the eyes eating sond
;;;
EyesEatenSoundTable
        dc.b 249,249,249,249,248,248,247,247,246,246,245,244,243,242,0
;;;
;;; pacman powerpill sound
;;; 
isr4 subroutine
        ldx #5
        ;;figure out if there are any eyes in the
        ;; maze
.loop0        
        lda Sprite_mode,X
        cmp #modeEaten
        beq .eyessound
        dex
        bpl .loop0
.noeyes        
        TableSoundPlayer PowerPillPtr,36876,PowerPillTable,isr4_reset
        
.eyessound
        TableSoundPlayer PowerSnd2Idx,36876,EyesEatenSoundTable,whatever
        
#if 0        
        ldy PowerPillPtr
        lda PowerPillTable,Y
        beq isr4_reset
        sta 36876
        inc PowerPillPtr
        rts
isr4_reset
        sta PowerPillPtr        ;reset index to 0
        rts
#endif        
;;; bonus life sound
isr6 subroutine
        dec BonusSound
        bne .done
        lda 36875
        beq .off
        lda #0
        beq .on
.off
        lda #249
.on        
        sta 36875
        lda #bonusInterval
        sta BonusSound
        inc BonusAwarded
        lda #bonusInterval+15
        cmp BonusAwarded
        bne .done
        lda #0
        sta BonusSound
        sta 36875
.done        
        rts
;;; sound for when pacman eats a fruit
;;; 
isr5 subroutine
        ldy FruitPillPtr        ;load note table index
        lda FruitSound,Y        ;load value from note table
        sta 36874               ;store to sound register
        beq isr5_reset          ;if read a 0 then reset
        inc FruitPillPtr        ;move to next note
        rts
isr5_reset
        sta FruitSoundOn        ;turn on or off fruit eating sound
        sta FruitIsOut
        sta FruitPillPtr        ;reset index to 0
        rts
        ;; 
        ;; update the jiffy clock manually
        ;;
        MAC UpdateJiffyCounter
;        clc
        INC JIFFYL
        BNE .dd
        INC JIFFYM
        BNE .dd
        INC JIFFYH
.dd        
        ENDM
        ;;
        ;; ISR for playing the death sound
        ;; 
DeathISR subroutine
        ldx DeathSoundPtr
        lda DeathSound,X
        sta 36876
        bne .skip
        jsr uninstall_isr       ;sound is finished remove ISR
.skip        
        inc DeathSoundPtr
        jmp isr2_done2
;;; 
;;; main entry for interrupt driven sound
;;; pacman eating sound, or power pill on sound
;;; or possible fruit eating sound
;;; also flashes the power pellets by rewriting the chargen data
;;; 
isr2 subroutine
        sei                     ;disable interrupts while in interrupt
        ;; I don't see why above is needed?
;        dec FrameLock
        ;; 
        ;; flash the power pellets
        dec PwrFlashCnt
        bne .00
        ;; zero out the character to make it blank i.e. flash
        ldx #flashRate
        stx PwrFlashCnt
        ldx #6
.next1
        lda PwrFlashSt
        beq .blankit
        lda BIT_PWR0+1,X
.blankit                        ;blank the power pellet
        sta BIT_PWR1+1,X
        dex
        bpl .next1
        Invert PwrFlashSt
.00
        lda BonusSound
        beq .not_bonus
        lda #249
        sta 36875
        jsr isr6                ;play the bonus pacman sound
        bne .not_power
.not_bonus        
        lda FruitSoundOn        ;should be be playing fruit eaten sound?
        beq .not_fruit
        jsr isr5
        ;jmp .done2
.not_fruit        
        
        lda POWER_UP            ;are we powered up?  
        beq .not_power
        jsr isr4                ;power up sound
        jmp .waka               ;skip the siren sound
.not_power
        jsr isr3                ;run the siren
.waka        
        lda eat_halted          ;is waka sound stopped?
        beq isr2_done2          
        ldx WakaIdx             ;if we are at the end of the waka
        bmi .reset              ;sound table then reset it
.0        
        ldy WakaTable,X         ;load next note of waka sonud
        sty wakavoice
        dex                     ;update waka sound index
        stx WakaIdx
isr2_done2
        lda POWER_UP            ;should we play power pill sound?
        bne .notpower2
        ;; manually update jiffy clock
        UpdateJiffyCounter
.notpower2        
        jmp defaultISR
.reset
        lda eat_halt            ;are we commanded to stop the eating sound?
        bne .nothalted
        sta eat_halted          ;A is 0
        sta wakavoice           ;stop noise channel with 0
        beq isr2_done2
.nothalted        
        ldx #WakaTableEnd-WakaTable
        bne .0                  ;jmp .0
;;; 
;;; silence all sound voices
;;; 
stopSound subroutine
        lda #0                   ;stop all sound channels
        sta voice4
        sta voice3
        sta voice2
        sta voice1
        rts
;;;
;;; remove current sound service routine
uninstall_isr subroutine
        sei
        store16 isr2_done2,$0314 ;replace standard isr
        cli                     ;reenable interrupts
        jmp stopSound           ;rts for us
;        rts
;;;
;;; install a sound service routine
install_isr SUBROUTINE
        sei
;        store16 isr1,$0314
        store16 isr2,$0314
;        store16 isr4,$0314
;        store16 isr3,$0314
        cli
        rts
;;; wrap a volume envelope
;;; around a specified delay
;;; S2 = delay
;;; s4 = raise pitch mode
delay SUBROUTINE
        txa
        pha
        ldx #0
.xloop        
        lda VolTable,X
        beq .done
        sta volume
        inx
        ldy #190
.yloop
        nop
        nop
        nop
        dey
        bne .yloop
        beq .xloop
.done        
        pla
        tax
        rts
;;;
;;; ghost eaten sound
SoundGhostEaten SUBROUTINE
        saveAll
        lda S2
        pha
        jsr uninstall_isr           ;turn off all sound but this


        ldx #195
.loop
        jsr delay

        stx 36876
        inx
        inx

        cpx #230
        bcs .done
        jmp .loop
.done
        lda #0
        sta 36876
        
        jsr install_isr
        lda #defaultVol
        sta volume

        pla
        sta S2
        resAll
        rts
        ;; 
        ;; perform a C style longjmp
        ;; no return from this method
longJmp subroutine
        ldx ResetPoint
        txs
        jmp PacDeathEntry

        MAC ClearPacSite
        ;; lda Sprite_sback
        ;; pha
        ;; lda Sprite_sback2
        ;; pha
        lda #EMPTY
        sta Sprite_sback
        sta Sprite_sback2
        ENDM

        MAC REstorePacSite
        pla
        sta Sprite_sback
        pla
        sta Sprite_sback2
        ENDM
        ;; Wait for a keypress
        MAC WaitKey
.wait        
        lda CURKEY
        cmp #{1}
        bne .wait
        ENDM
;;; 
;;; called when pacman touches a ghost
;;; performs death animation
;;; is responsble for restoring correct playfield tiles on exit
;;; which needs some work :(
death subroutine
        jsr uninstall_isr
        jsr PowerPillOff           ;call off routine to clean up
        jsr EatSoundOff            ;stop waka
        WaitTime 1                 ;delay for 2 second 

        ClearPacSite
	;; perform some animation
	lda #0                     ;animation frame is pac mouth open
	sta Sprite_frame
        sta DeathSoundPtr
	store16 BIT_DEATH1,Sprite_src
.loop0
        ldx DeathSoundPtr
        lda DeathSound,X
        beq .done
        inc DeathSoundPtr
        cmp #2
        beq .nextFrame
        
        sta 36876
.ll
        lda VICRASTER
        bne .ll
.wait2        
        lda VICRASTER
;        cmp #13
;        bcs .wait2
        cmp #70
        bne .wait2
        
        
        jmp .loop0
.nextFrame        
	ldx #0                  ;select pacman sprite
	jsr render_sprite       ;render bits
	Invert Sprite_page      ;
	ldx #0                  ;select pacman sprite
	jsr drwsprt1            ;place tiles on screen

        inc Sprite_frame
        bne .loop0
.done
        lda #1
        jsr WaitTime_
;        RestorePacSite
        jsr stopSound
        jsr DecrementLives
        JmpReset modePacDeath   ;reset level, pacman died mode

        ;; set up dot counts for releasing ghosts
        ;; this is self modifying code, as we are overwriting
        ;; the cpy instruction's argument elsewhere in the code
        ;; need to clean this up
        ;; 41 is totalDots/4
        ;; totalDots/8 = 20
        ;; /16 = 20
        MAC SetupDotCounts
        lda DOTCOUNT
        clc
        lsr                     ;2
        sta XBlinkyS1+1
        lsr                     ;4
        sta ClydeDots
        sta XBlinkyS2+1         ;blinky to full speed at 75% of dots eaten
        lsr                     ;8
        sta PinkyDots
        lsr                     ;16
        sta InkyDots
        ;;
        lda DOTCOUNT
        tay
        sec
        sbc InkyDots            ;subtract inky dots from totalDots
        sta XInkyDots+1         ;store in compare instruction
        tya
        sbc PinkyDots           ;subtract pinky dots from totalDots
        sta XPinkyDots+1        ;store in compare instruction
        tya
        sbc ClydeDots           ;subtract clyde dots from totalDots
        sta XClydeDots+1        ;store in compare instruction
;        jsr WaitFire
        ENDM

        MAC ResetSpriteLocs 
        store16 pacStart , Sprite_loc2+[2*0]
        store16 g1Start  , Sprite_loc2+[2*1]
        store16 g2Start  , Sprite_loc2+[2*2] 
        store16 g3Start  , Sprite_loc2+[2*3] 
        store16 g4Start  , Sprite_loc2+[2*4]
        ;; reset pacman sprite offset and tile
        ;; ldx #0
        ;; stx Sprite_offset2
        ;; jsr render_sprite
        ENDM
        ;; modify the main game loop
        ;; to not include the extra snippet
        ;; of code used only during the Actor
        ;; introduction mode and initial 'intro tune'
        MAC ModifyMainLoop
        store16 MainLoop0,MainLoopEnd+1
        ENDM
        MAC ResetMainLoop
        store16 IntroLoop,MainLoopEnd+1
        ENDM

AllSoundOff subroutine
        jsr uninstall_isr
        jsr PowerPillOff
        jsr EatSoundOff            ;waka off
        rts
;;;
;;;determine correct ghost speed based on levels complete
;;; output: ghost speed in A
;;;
        MAC GetTunnelSpeed
        saveX
        ldx #6
        jsr GetSpeed
        resX
        ENDM
        MAC GetGhostSpeed
        saveX
        ldx #4
        jsr GetSpeed
        resX
        ENDM
        MAC GetPacSpeed
        saveX
        ldx #0
        jsr GetSpeed
        resX
        ENDM
;;;
;;; determine correct speed based on levels complete
;;; input:
;;; X=4 get ghost speed
;;; X=0 get pacman speed
;;; output: speed in A
GetSpeed SUBROUTINE
        lda LevelsComplete
        cmp #1
        bmi .level1
        cmp #2
        bmi .level2
        cmp #5
        bmi .level5
        lda Lvl21Spds,X
        rts
.level5
        lda Lvl5Spds,X
        rts
.level2
        lda Lvl2Spds,X
        rts
.level1
        lda Lvl1Spds,X
        rts
;;; 
;;; reset game after pacman death, or level start
;;; inputs: S1==#modeResetGame draw the maze and do all initialization for a new level
;;; S1==modeEndLevel : draw the maze but do not reset the lives, etc
;;; outputs:
;;; Sprite_page=0
;;; and a lot more side effects :(
reset_game subroutine
        ResetMainLoop        ;reset main loop to show intro
        jsr AllSoundOff
        
        lda S1               ;load 'mode' of reset
        beq .pacdeath        ;skip resetting dot count if it's a death
        ldx #totalDots       ;reset dot counter for end level
        stx DOTCOUNT         ;and game reset modes

        cmp #modeResetGame
        bne .endlevel
        jsr reset_game1      ; full game reset
.endlevel
        jsr end_level
.pacdeath        
        ResetSpriteLocs
        ;; init loop counter
        ldx #SPRITES-1       ;SPRITES is 1 based, so -1
.0
        jsr erasesprt

        lda inBoxTable,X
        sta Sprite_offset2,X
        sta Sprite_offset,X
        lda Sprite_turnbase,X
        sta Sprite_turn,X
        ;; initialize a bunch of variables that can be 0
        ;; on start
        lda #0
;        sta FrameLock
        sta JIFFYL              ;game timer to 0
        sta JIFFYM
        sta JIFFYH
        sta PowerSnd2Idx
        sta SirenOffset
        sta BonusSound
        sta PwrFlashSt
        sta POWER_UP            ;power pill off
        sta WakaIdx
        sta eat_halted
        sta eat_halt
        sta FruitIsOut
;        sta Sprite_turn,X
        sta Sprite_page
        sta Sprite_frame,X
        sta Sprite_sback,X
        sta Sprite_mode,X
        lda #dirHoriz
        sta Sprite_dir2,X
        cpx #0             ;are we pacman, then
        beq .skip_bmap     ;skip setting ghost bitmap and speed
        GetGhostSpeed
        sta Sprite_base,X  ;load base sprite speed
        sta Sprite_speed,X ;store it
        store16x GHOST,Sprite_src ;set bitmap for ghosts
.skip_bmap
        dex
        bpl .0
        GetPacSpeed
        sta Sprite_base
        sta Sprite_speed
;;; not needed?? pacman goes left through joystick
;        lda #motionLeft
;        sta Sprite_motion
        lda #motionRight
        sta Sprite_motion+1
#ifconst LARGEMAZE        
        sta Sprite_motion+3
#else
        lda #motionUp
        sta Sprite_motion+3
#endif
        lda #motionLeft
        sta Sprite_motion+2
        sta Sprite_motion+4
        ;; sprites not in ghost box: pacman and blinky
        lda #modeOutOfBox
        sta Sprite_mode+2
        lda #modePacman
        sta Sprite_mode+0

        ;; erase any fruits that may have been left out
        lda #EMPTY              ;blank tile
        sta screen+cherryRow*22+cherryCol
        
        lda #flashRate          ;power pill flash timer init
        sta PwrFlashCnt
        lda #8                  ; black for
        sta 36879               ; border and screen colors
        sta volume              ; turn up the volume to 8
        lda #!JOYL
        
        sta LASTJOYDIR
        lda #1
        sta SirenDir
        sta FrameLock
        sta PACFRAMED
        
        lda #modeLeaving
        sta Sprite_mode+1       ;inky leaves right away
#if 0
        ;; store the time the level was started
        lda JIFFYL
        sta LevelStartTm
        lda JIFFYM
        sta LevelStartTm+1
#endif
        
.continue        
        SetupDotCounts          ;calc when to release ghosts
        jsr initChaseTimer      ;reset ghost chase/scatter timer
        jsr DisplayLives        ;show lives meter
        
        rts
;;; display the fruit/level meter on the left
DisplayLevelMeter subroutine
        store16 screen+22,W1    ;screen pointer
        store16 clrram+22,W2    ;color ram pointer
        ldx #MAXLEVELCHERRIES   ;loop counter
        lda LevelsComplete
        sta S1                  ;temp counter
        inc S1
        ldy #0                  ;indexed addr to 0
.0
        lda S2
        lda #EMPTY              ;empty tile to clear cherries if any are present
        dec S1                  ;cherries drawn counter
        bmi .blank                  ;skip drawing cherry
        lda #CHERRY
.blank        
        sta (W1),Y              ;cherry on screen
        lda #RED
        sta (W2),Y              ;make red in color ram
        add16Im W1,#22          ;bump screen ptr
        add16Im W2,#22          ;bump color ptr
        dex                     ;dec loop counter
        bpl .0                  ;while not done
.done        
        rts
;;; level game reset
;;; or possibly full game reset
;;; #modePacDeath or #modeResetGame
;;; 
end_level subroutine
        jsr mkmaze2
        jsr DisplayScore
        lda #GHOST_WALL
        sta [[outOfBoxRow+1]*22]+outOfBoxCol+screen
        lda #CYAN
        sta [[outOfBoxRow+1]*22]+outOfBoxCol+clrram

;        jsr initChaseTimer
        jsr ResetBlinky

        ;; modify difficulty settings based on level
        lda LevelsComplete
        dec Agonizer
        bne .1
        lda #2
        sta Agonizer
        ;; make level harder by increasing ghost chase-mode
        ;; durations in ChaseTable
        ldx ChaseTableSz-1
.0
        move16x ChaseTable,W1
        sub16Im W1,[1*softTimerRes]
        cmp16Im W1,#0           ;did this number hit 0?
        bne .not0
        store16 #2,W1           ;no lower on this number
        move16x2 W1,ChaseTable
.not0
        dex                     ;word pointer decrement
        dex
        bpl  .0                 ;while > 0
        lda PowerPillTime
        sec
        sbc #60                 ;decrease power pill time
        bcs .pos                ;have we hit bottom?
        lda #2                  ;then store min value
.pos                            ;else
        sta PowerPillTime       ;store new value
.1
        ;; increase the value of fruits that are eaten by 300
        sei
        sed
        clc
        lda fruitPoints+1
        adc #03
        sta fruitPoints+1
        cld
        cli
        
        inc LevelsComplete      ;inc levels complete counter
        jsr DisplayLevelMeter   ;show cherries for level
        rts
#ifconst LARGEMEM        
;;; row 5 for panicman logo
;;; fantasies of making a real intro, but out of time
;;; and out of memory in +3k version :(
Vanity SUBROUTINE
        jsr AllSoundOff
    
        lda #8
        sta 36879
        ;; 
        store16 clrram,W2
        store16 screen,W1
        ldy #0
.loop0
        cmp16Im W2,clrram+22*23
        beq .0
        lda #BLUE
        sta (W2),Y
        lda #EMPTY
        sta (W1),Y
        inc16 W2
        inc16 W1
        jmp .loop0
.0        
        
        store16 screen+(5*22)+3,W1
        store16 panicman_msg,W2
        jsr ndPrint
        store16 dbills_msg,W2
        store16 screen+(7*22)+3,W1
        jsr ndPrint
        store16 screen+(9*22)+3,W1
        store16 jmessner_msg,W2
        jsr ndPrint
        jsr WaitFire
        rts
#endif        
;;; full game system reset
;;; called after game over
;;; 
reset_game1 subroutine
        lda #STARTLEVEL
        sta LevelsComplete
        lda #basePowerTime
        sta PowerPillTime
        lda #0
        sta BonusAwarded
        sta fruitPoints
        sta fruitPoints+1
        sta PlayerScore_l
        sta PlayerScore_m
        sta PlayerScore_h
        sta POWER_UP            ;power up to 0

        lda #2
        sta Agonizer

        lda #pacLives           ;1 based : number of lives + 1
        sta PacLives
;BLARGO1
        ldx #4
.loop
        move16xx Div22Table_i,Div22Table
        dex
        bpl .loop
        
        rts
        ;; game over macro
        ;; display message and restart
        MAC GameOver
        store16 screen+leftMargin+22*msgRow+21/2-gameover_msg_sz/2,W1      ;post the player ready message
        store16 gameover_msg,W2
        jsr ndPrint
        jsr WaitFire
        jsr rsPrint
        JmpReset modeResetGame    ;longjmp to reset of game
        ENDM
;;; 
;;; bonus life routine
IncrementLives subroutine
        inc PacLives
        sei
        lda #bonusInterval
        sta BonusSound
        sta BonusAwarded
        cli
        jsr DisplayLives
        rts
;;; 
;;; -1 pacman lives
;;; and update dislay
;;; 
lifeStart equ 22*[22-5]         ;location on left of screen to show lives meter
;;; 
DecrementLives subroutine
#ifnconst UNLIMITED_LIVES        
        dec PacLives
        bne .done
        ;; game is over
        GameOver
#endif        
.done        
        rts
;;; display pacman icons in the lower left
;;; to show remaining lives
;;; 
DisplayLives subroutine         ;entry point for displaying lives only
        txa
        pha
        ldx PacLives
        dex                     ;-1 don't display current life
        ldy #maxLives*22
.0
        cpx #0
        bcc .skip
        beq .skip
        lda #PACLIFE             ;load 'life' character tile
        sta screen+lifeStart,Y
        lda #YELLOW
        sta clrram+lifeStart,Y
        jmp .cont
.skip        
        lda #EMPTY
        sta screen+lifeStart,Y
        lda #BLACK
        sta clrram+lifeStart,Y
.cont        
        tya
        sec
        sbc #44                 ;move down screen 2 spaces
        tay
        dex
        bpl .0
.done
        pla
        tax
        rts
;;; show the ghost and pacman as quickly as possible
;;; used by the intro music 
ActorIntro subroutine
#ifconst ACTORINTRO        
        store16 screen+leftMargin+22*msgRow+21/2-ready_msg_sz/2,W1      ;post the player ready message
        store16 ready_msg,W2
        jsr ndPrint             ;show message


        lda LevelsComplete      ;are we after the first level
        bne .skipsong           ;then no song
        lda DOTCOUNT            ;is the dotcount
        cmp #totalDots          ;less than total, then we died midlevel
        bcc .skipsong           ;and no song
;        jsr WaitFire
        jsr player              ;play pac intro song
        jmp .cont
.skipsong                       ;unless we played the pacsong
        WaitTime 2              ;delay for 2 second after ready msg
.cont
        jsr rsPrint             ;restore screen where text was printed
#endif    
        ;; move jmp in main loop to proper
        ;; spot for running the game
        ModifyMainLoop
        
        jsr install_isr         ;allow game sound interrupts
        jsr initChaseTimer
        
        rts
;-------------------------------------------
; MAIN()
;-------------------------------------------
main SUBROUTINE
#if 0        
        lda #8
        sta 36879               ; border and screen colors
        sta volume              ; turn up the volume to 8
        cli
        jsr player
#endif        
#if 0
        lda #$ea
        STA BCD
        ldx #0
        jsr DisplayBCD
        brk
#endif

.try_again        
        lda $a2                 ;jiffy as random seed
        beq .try_again          ;can't be zero
        sta r_seed
        
        sei                     ;disable interrupts
;        store16 maingo,$0316    ;on error, restart game
        ldx #$ff                ;init stack to top of page 1
        txs
        stx ResetPoint
        store16 defaultISR,$0314 ;replace standard isr with minimal ISR ( jiffy clock only )
        lda #$7f
        sta $911e               ;disable nmi

        lda #127
        sta VIA2DDR             ;setup VIA for joystick read

        lda #0
        sta $9113               ;joy VIA to input

        lda VICSCRN
#ifconst LARGEMEM
        ora #%1101              ;$1400 char ram
        sta VICSCRN
        jsr copychar
#else        
;        and #$f0
        ora #$0f                 ;char ram pointer is lower 4 bits
#ifnconst MATRIX        
        sta VICSCRN
#endif        
#endif        
        ;; build the siren table
        lda #sirenBot
        sta S0
        ldx #0
.loop                           ;build table up
        lda S0
        sta SirenTable,X
        inc S0
        inx
        cpx #sirenTop-sirenBot
        bne .loop
        
.loop2                          ;build table down
        lda S0
        sta SirenTable,X
        dec S0
        inx
        cpx #((sirenTop-sirenBot)*2)
        bne .loop2
        lda #0
        sta SirenTable,X
#ifconst LARGEMEM
        jsr Vanity
#endif
        lda #modeResetGame      ;ask reset game to do full reset
        sta S1                  ;arg to reset_game below
PacDeathEntry                   ;code longjmp's here on pacman death
        jsr reset_game
        ;; Sprite_page=0 now
        
        jmp .background         ;sneakily enter the main game loop halfway through
        ;; I don't even remember why anymore :(

IntroLoop
        ;; checking for Sprite_page assures we make at least 2 passes
        ;; through here, since it gets inverted on each pass
        lda Sprite_page         ;sprites havn't been rendered yet?
        beq MainLoop0           ;yes,go around one more time
        jsr ActorIntro          ;message and song
MainLoop0
        
#ifconst MASTERDELAY        
        lda #masterSpeed
        sta MASTERCNT
#endif

.iloop        
        lda VICRASTER           ;load raster line
        bne .iloop

.cont        
#ifconst MASTERDELAY        
        dec MASTERCNT
        bne .iloop
#endif        
        ;; ok, we are at vertical blank, on one of the frames we want to render
        ;; here we go ...

        Invert Sprite_page      ;dbl buffering, switch sprite tiles

        ;; decrement game based timers such as power pills and attack/scatter timer
        ldy POWER_UP
        beq .skip
        dey
        sty POWER_UP
        bne .skip
        jsr PowerPillOff
.skip
        HasTimerExpired TIMER1,Timer1Expired        ;test if Timer1 expired and notify

;        Display1 "P",0,PowerPillTime
;        Display2 "T",0,TIMER1+1,TIMER1
;        Display2 "J",5,JIFFYM,JIFFYL
;        Display1 "J",0,JIFFYL
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
        ldy Sprite_dir,X
        lda (W1),Y
        sta Sprite_back2,X
        ;; end collection of background tiles
        jmp .backloop
        
.draw
        InitSpriteLoop          ;foreach sprite
.drawloop
        dec SPRITEIDX                
        bmi .player
        ldx SPRITEIDX
        jsr drwsprt1            ;draw in new location
        jmp .drawloop
.player
        ;; take care of fixing the fruit colors
        ;; and aging the fruit
        lda FruitIsOut          ;is a fruit displayed?
        beq .player1

        lda #RED                ;yes, restore its color from sprites that destroy it
        sta clrram+cherryRow*22+cherryCol
#ifconst AGEFRUIT
        ;; age the fruit, so it dissappears after a while
        lda Sprite_page
        beq .player1            ;only age fruit every other frame
        dec FruitIsOut
        bne .player1            ;fruit is still good, no worries
        ;; fruit is spoiled , removed it
        lda #EMPTY              ;blank tile
        sta screen+cherryRow*22+cherryCol
#endif        
.player1        
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
        bmi MainLoopEnd
        ldx SPRITEIDX
        jsr CompositeSprite
        ldx SPRITEIDX
        jsr render_sprite
        jmp .playerloop

MainLoopEnd
;        Display1 "D",0,DOTCOUNT
        jmp IntroLoop

PelletRow equ 3
;;; place power pellets
;;; W1 pointer
        MAC placePellets
        sta {1}+22*PelletRow + 2
        sta {1}+22*PelletRow + 22-2
        sta {1}+22*17+2
        sta {1}+22*17+22-2
        ENDM
;;; split a byte into nybbles
;;; A=input
;;; A=low x=high
SplitByte subroutine
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        pla
        and #$f
        rts

set_color subroutine
        ldx #WHITE              ;assume white
        cmp #DOT                ;is it a dot?
        beq .isdot              ;yep, skip to store in clrram
        ldx #BLUE               ;oh, its a wall
.isdot
        txa                     ;color to A
        sta (W3),Y              ;write to clrram
        rts
        
        INCLUDE "audio.asm"

;;; 
;;; locals for maze generation routine
dataoffset equ S1
mirror     equ S2
#ifconst LARGEMAZE
mzRightEdge equ 20
mzLeftEdge  equ 1        
mzMiddle    equ 10        
        INCLUDE "maze.asm"
#else        
mzRightEdge equ 18
mzLeftEdge  equ 2        
mzMiddle    equ 9        
        INCLUDE "smmaze.asm"
#endif        
;;; process nibble from compressed maze data
process_nibble2 subroutine
        ldy mirror

        pha
        clc
        adc #6                  ;add offset to create tile #
        sta (W2),Y              ;put it on the screen
        jsr set_color
        cpy #mzMiddle           ;reached middle of screen?
        beq .0
        ;; place mirror side

        lda #mzRightEdge        ;mazeEdge-Y

        sec
        sbc mirror
        tay                     ;y = 21 - y
        pla                     ;pull tile back out
        tax                     ;save in X for a bit
        ;; is it a reversible character?
        and #%00001000          ;check bit 3 and 4 not set
        bne .notrev     
        txa                     ;restore A
        eor #%00000100          ;clear bit 3
        jmp .store
.notrev
        txa
.store        
        clc
        adc #6
        sta (W2),y
        jsr set_color

        inc mirror
        
        rts
.0
        pla
        ;; time to move to the next line
        add16Im W2,#22
        add16Im W3,#22
        lda #0
        sta mirror
        rts
;;; draw maze
;;; restore DOTCOUNT
mkmaze2 subroutine
        store16 screen,W2
        ;; clear top line, and left line
        ldx #22
.loop   
        lda #EMPTY
        sta screen,x
        sta (W2),y
        lda #WHITE
        sta clrram,x
        add16Im W2,#22
        dex
        bpl .loop
        ;; end clearing of top and left lines
        
        store16 MazeB,W1
        store16 screen+22+mzLeftEdge,W2
        store16 clrram+22+mzLeftEdge,W3
        lda #0
        sta dataoffset
        sta mirror
.fetch_byte
        ldy dataoffset
        cpy #MazeX-MazeB
        bne .begin

        lda #totalDots
        sta DOTCOUNT            ;dot count to 0
        lda #PWR
        placePellets screen
        lda #WHITE
        placePellets clrram
        sta clrram+[22*pacStartRow]+pacStartCol
        rts
.begin
        lda (W1),Y              ;fetch high nibble
        jsr SplitByte
        pha                     ;push low nibble
        txa                     ;high nibble to A
        jsr process_nibble2
        pla                     ;pull low nibble
        jsr process_nibble2
        inc dataoffset
        bne .fetch_byte         ;jmp .fetch_byte
        
        rts
#if 1
;;; waits for joystick to be pressed
;;; and released
WaitFire SUBROUTINE
.loop        
        lda JOY0                ; read joy register
        tay
        and #JOYT               ;was trigger pressed?
        beq .loop1
        bne .loop
.loop1                          ;wait for trigger to be released
        lda JOY0
        and #JOYT
        bne .fire
        beq .loop1
.fire
        rts
#endif
scroll_down SUBROUTINE
        lda #motionDown
        sta SPRT_LOCATOR
        lda #8
        sta END_SCRL_VAL
        lda #1
        sta SCRL_VAL
        jmp ScrollVertical        ;rts for us

scroll_up SUBROUTINE
        lda #motionUp
        sta SPRT_LOCATOR
        lda #0
        sta END_SCRL_VAL
        lda #-1
        sta SCRL_VAL
        jmp ScrollVertical        ;rts for us


ScrollLeft SUBROUTINE
        lda #motionLeft
        sta SPRT_LOCATOR
        lda #0
        sta END_SCRL_VAL
        lda #$ff                ;-1 into A
        sta SCRL_VAL
        jmp ScrollHoriz        ;rts for us

scroll_right SUBROUTINE
        
        lda #motionRight
        sta SPRT_LOCATOR
        lda #8
        sta END_SCRL_VAL
        lda #$01
        sta SCRL_VAL
        jmp ScrollHoriz        ;rts for us


        MAC MoveGhost
        lda Sprite_mode,X
        beq .done               ;ghost in box don't get to move
        jsr MoveGhostI
        lda Sprite_mode,X
        cmp #4                          ;< 4 no copy needed
        bcc .done
        move16x2 W3,Sprite_src
.done        
        ENDM
;;; move ghost in its currently indicated direction
;;; output: W3 pointer to ghost bitmap for selected direction
MoveGhostI SUBROUTINE
#ifconst GHPLAYER
        store16 GHOST,W3
        rts      ;keyboard is controlling ghost
#endif
        lda Sprite_motion,X
        cmp #motionRight
        beq .right
        cmp #motionDown
        beq .down
        cmp #motionLeft
        beq .left
        cmp #motionUp
        beq .up
;        brk
        rts
.left
        store16 GHOST,W3
        jmp ScrollLeft
.right
        store16 GHOSTR,W3
        jmp scroll_right
.up
        store16 GHOSTR,W3
        jmp scroll_up
.down
        store16 GHOST,W3
        jmp scroll_down
        
#ifconst GHPLAYER
;;; move a ghost using the keyboard
;;; what are the keys: a,w,d,x
GhostAsPlayer SUBROUTINE
        cpx #blinky   ;only move blinky
        beq .0   
        rts
.0        
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
        jsr ScrollLeft
        rts
.right
        jsr scroll_right
        rts
.up
        jsr scroll_up
        rts
.down
        jsr scroll_down         ;
        rts
#endif
#if 0        
keypressed dc.b 0        
SpecialKeys SUBROUTINE
        saveX
        lda 197
        cmp #9                  ;'w'
        bne .done


;        ldx #inky
;        jsr LeaveBox
        lda keypressed
        bne .done
        ldx #blinkyCruise1
        jsr IncreaseBlinky
        lda #1
        sta keypressed

.done        
        resX
        rts
#endif
        
        MAC UpdateMotion 
        lda GHOST_DIR
        sta Sprite_motion,X
        ENDM
        ;; {1} as screen location
        ;; broken into col,row
        ;; stored in {2} , {3}
        ;; uses: W1
        MAC ScreenToColRow
        sub16Im {1},screen
        jsr Divide22_16
        lda W1                  ;column result ( remainder )
        sta {2}
        lda DIV22_RSLT          ;row result
        sta {3}
        
        ENDM
        ;; calculate the ghost in X's
        ;; about to be display row and column
        ;; Output: GHOST_ROW,GHOST_COL
        MAC CalcGhostRowCol
        
        move16x Sprite_loc,W1
        ScreenToColRow W1,GHOST_COL,GHOST_ROW
        
        ENDM
        ;; pac upcoming row col into variables
        ;; OUTPUT: PACCOL,PACROW
        MAC CalcPacRowCol
        ;; alternate to save a few bytes from below
        lda Sprite_loc
        sec
        sbc #screen&$ff
        sta W1
        lda Sprite_loc+1
        sbc #screen>>8
        sta W1+1
        ;; 
;        move16 Sprite_loc2,W1
;        sub16Im W1,screen
        ;;      
        jsr Divide22_16
        lda W1
        sta PACCOL             ;store tile column
        asl                    ;multiply by 8
        asl
        asl
        ldy Sprite_dir         ;are we oriented vertically?
        cpy #dirVert
        beq .vert              ;yes, don't add offset into X pixel
        clc                    ;we are horiz
        adc Sprite_offset      ;add in pixel count to X
.vert
        sta PACXPIXEL          ;store pixel column
        
        lda DIV22_RSLT         ;get row result from division
        sta PACROW             ;store tile row
        asl                    ;multiply by 8
        asl
        asl
        cpy #dirHoriz          ;are we horizontall
        beq .horiz             ;yes, don't add offset into Y
        clc                    ;we are vertical
        adc Sprite_offset      ;add in the smooth scroll offset to pixel count
.horiz        
        sta PACYPIXEL           ;store pixel row
        ENDM
        ;; 
        ;; check if sprite X gets to move this frame
        ;; and branch to {1} if allowed
        MAC MyTurn2
        lda Sprite_turn,X
        sec
        sbc Sprite_speed,X
        sta Sprite_turn,X
        beq .skip
        bcs {1}
        ;; we don't get to move this turn
    ;; reset the turn counter
.skip    
;        clc
        adc #speedBase
        sta Sprite_turn,X

        ENDM
        ;; Place target tile for an eaten ghost
        ;; into GHOST_TGTCOL,GHOST_TGTROW
        ;; returns: Z not set
        MAC SetEatenTargetTile
        lda #outOfBoxCol
        sta GHOST_TGTCOL
        lda #outOfBoxRow+2
        sta GHOST_TGTROW
        ENDM
        ;; Place target tile for ghost leaving box 
        ;; into GHOST_TGTCOL,GHOST_TGTROW
        ;; returns: Z not set
        MAC SetLeavingTargetTile
        lda #outOfBoxCol
        sta GHOST_TGTCOL
        lda #outOfBoxRow
        sta GHOST_TGTROW
        ENDM
somertn        
        rts
;;; run AI for all ghosts
GhostAI SUBROUTINE
        lda #0
        sta CORNER_SHAVE        ;ghosts get no cornering bonus
        ;; caculate pacman row,col so ghosts can use
        ;; in their AI routines
        CalcPacRowCol

        ldx #5
.loop
        dex
        beq somertn            ;pacman is sprite 0, so we leave
ailoop0
        MyTurn2 GhostTurn      ;does this ghost get to move this time?
        jmp .loop        
GhostTurn
        lda Sprite_mode,X
        cmp #modeReverse
        beq .reversing
        cmp #modeLeaving
        beq .leaving
        cmp #modeEaten
        beq .eaten
        cmp #modeFright0        ;first fright?
        bne .next
        lda #modeFright         ;reverse direction and go to second fright
        bne .reversing2
.next                           ;second fright?
        cmp #modeFright
        bne .notfrightened
        jsr FrightAI
        jmp .continue
.reversing
        lda #modeOutOfBox       ;cancel #modeReverse
.reversing2                     ;
        sta Sprite_mode,X
        jsr ReverseDirection    ;calc opposite direction in A
        sta Sprite_motion,X
        bne .moveghost          ;jmp .moveghost
.eaten                          ;load target tile for eaten ghosts
        SetEatenTargetTile
        bne .continue
.leaving                        ;load target tile for ghosts leaving box
        SetLeavingTargetTile
        bne .continue
.notfrightened        
#if 1
        ;; check if we should bother with AI
        ;; we have to be at an intersection
        lda Sprite_offset,X
        beq .decide
        cmp #8
        bne .moveghost
        
.decide                         ;entering a new tile
#endif        
        lda CHASEMODE
        bne .chasing
        jsr ScatterGhostAI
        jmp .continue
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
        bne .continue
        jsr Ghost1AI
.continue
        ;; this is a hack due to some bugs in the AI routine
        ;; for inky right now -- check if the AI decided upon a move
        ;; if not, keep going in the previous direction
        lda Sprite_motion,X
        sta PrevSprtMotion
        jsr PossibleMoves
        lda #noChoice
        cmp GHOST_DIR
        bne .isok
        lda PrevSprtMotion
        sta GHOST_DIR
.isok        
;        cpx #focusGhost
;        bne .notfocus
;        Display1 "G",17,GHOST_DIR
.notfocus

        UpdateMotion            ;up eyes for direction
;        jsr SpecialKeys
.moveghost
#ifconst GHPLAYER        
        jsr GhostAsPlayer
#else        
        MoveGhost
#endif        
        jsr Collisions
.animate
        ;; animate the ghost by changing frames
        dec Sprite_frame,X
        bpl .done
        lda #1                  ;reset frame counter
        sta Sprite_frame,X
.done
        jmp .loop
;;; 
;;; open the door on the ghost box
;;; register input:
;;; X = ghost to leave
;;; X,Y unmolested
;;; if the ghost is already out of the box then do nothing
LeaveBox SUBROUTINE
        lda #modeInBox
        cmp Sprite_mode,X       ;is ghost in box?
        bne .done               ;no, then already out, no need
        ;; make sure ghost has proper bitmap
        ;; in case if was the eaten eyes leaving the box
        store16x GHOST,Sprite_src

        ;; instruct ghost to leave box
        lda #modeLeaving
        sta Sprite_mode,X

        lda Sprite_base,X
        sta Sprite_speed,X
        
.done        
        rts                     ;
;;;
;;;take blinky out of cruise mode
;;; 
ResetBlinky subroutine
        sei
        lda #blinkyCruiseOff
        sta BlinkyCruise
        sta SirenOffset         ;should be 0
        cli
        rts
;;;
;;; Increase blinky speed to make things harder
;;; increase the panic inducing siren sound
;;; X = blinky mode
soundInc equ 3        
        MAC IncreasePanicLevel
        sei
        lda SirenOffset
        clc
        adc #soundInc
        sta SirenOffset
        cli
        ENDM
;;; increases the speed of blinky
;;; levels > 2 use the IncreaseBlinkyHard
;;; levels <=2 use this routine, and manipulate
;;; the sprite speed
;;; X: in, blinky cruise level
;;; X clobbered
IncreaseBlinky subroutine
        stx BlinkyCruise        ;store the current blinky cruise mode
        ;; levels > 2 are done with the IncreaseBlinkyI routine
        ;; otherwise blinky can be increased with Sprite_speed adjustments
        lda LevelsComplete
        cmp #harder2 ;no sense, ghosts are already fastest
        bcs .over2              ;ghosts are already fast
        cpx #1                  ;what cruise mode are we supposed to be entering?
        bne .cruise2
.cruise1        
        lda #Speed_standard
        bne .store              ;jmp .store
.cruise2
        lda #Speed_fast
.store
        ;; don't increase current speed if he is frightened
        ;; but rather the base speed
        ldx #blinky
        ldy Sprite_mode+blinky
        cpy #modeFright
        beq .fright
        sta Sprite_speed,x
.fright        
        sta Sprite_base,X
.over2
        IncreasePanicLevel
        rts
;;; note:
;;; self modifying code
;;; for blinky speed
;;; 
DotEaten SUBROUTINE
        saveX
        jsr SoundOn

        lda #$10
        ldy #$0
        jsr UpdateScore

        ;; get pacman dot speed
        ldx #1
        jsr GetSpeed
        sta Sprite_speed
.noslowdown        
        ;; 
        ldy DOTCOUNT
.01        
        lda #modeLeaving
XPinkyDots        
        cpy #pinkyDots
        bcs .0
        ldx #pinky
        jsr LeaveBox
.0
        cpy  #fruit1Dots
        bne .1
        jsr Fruit
.1        
        cpy #fruit2Dots
        bne .2
        jsr Fruit
.2
XInkyDots        
        cpy #inkyDots
        bcs .3
        ldx #inky
        jsr LeaveBox
.3
XClydeDots        
        cpy #clydeDots
        bcs .4
        ldx #clyde
        jsr LeaveBox
.4
        lda BlinkyCruise
        cmp #blinkyCruise2      ;already in cruise mode 2?
        beq .5                  ;yes, no checking needed
        cmp #blinkyCruise1
        beq XBlinkyS2
XBlinkyS1        
        cpy #[totalDots/2]      ;time to go into mode 1
        bcs .5                  ;nope, skip mode 1
        ldx #blinkyCruise1      ;select speed mode
        jsr IncreaseBlinky      ;invoke
        ldy DOTCOUNT            ;Y was clobbered by previous, remember this is dots remaining
XBlinkyS2
        cpy #[totalDots/3]      ;time to go into mode 2?
        bcs .5                  ;< dots needed  skip mode2
        ldx #blinkyCruise2      ;select speed mode into A
        jsr IncreaseBlinky      ;invoke
.5        
        resX
        rts
;;; return true ( Z=1 ) if character in A is a wall
;;; W2 ( candidate position )
IsWall SUBROUTINE
.checkWall
        cmp #RTOP
        bcc .ok                 ;less the RTOP tile
        cmp #EMPTY
        bcs .ok                 ;or greater than EMPTY
        ;; move isn't ok, set Z=1
        lda #0
        rts
.ok
        cpx #0
        bne .notpac
        ;; pacman gets check to see if he has eaten here already,
        ;; if so then turn off eating sound
        cmp #EMPTY
        bne .done1              ;we must be covering dots
        jsr EatSoundOff
.notpac        
        ;; move is ok, set Z=0
        lda #1
.done1
        rts
;;; 
;;; put a fruit out
;;; 
Fruit SUBROUTINE
        lda #CHERRY
        sta screen+cherryRow*22+cherryCol
SetFruitColor        
        lda #RED
        sta clrram+cherryRow*22+cherryCol
        lda #fruitTime                ;time fruit is out
        sta FruitIsOut
        rts
#if 0        
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
#endif        
;;; update {1} with min of A or {1} store SPRT_LOCATOR, which is the considered 'move'
;;; in {2} if it was the best move so far
;;; INPUTS: SPRT_LOCATOR
;;; locator is set from the scroll_up, scroll_down, etc routines
;;; a litte remote in locality of reference for the reader, not happy about it
;;; but there it is
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
        lda Sprite_offset,X
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
#if 0        
        Display1 "T",12,PACCOL
        Display1 "Y",15,PACROW
        Display1 "N",6,PACXPIXEL
        Display1 "M",9,PACYPIXEL
#endif        
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
        MAC SaveSprite 
        lda Sprite_offset2,X
        sta SAVE_OFFSET2
        lda Sprite_offset,X
        sta SAVE_OFFSET
        lda Sprite_dir,X
        sta SAVE_DIR
        lda Sprite_dir2,X
        sta SAVE_DIR2
        ENDM
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
        ;; {1}=display letter {2}=offset
        ;; displays distance from target tile
        MAC IfFocus
;;         cpx #focusGhost
;;         bne .not
;;         Display1 {1},{2},S3            ;
;; .not        
        ENDM
;;; X ghost to check
;;; the AI has determined a target tile for us
;;; this routine looks at every possible move we could make
;;; and figures out which one has the least Euclidean distance
;;; to the target tile
;;; locals: S3,S4,S0 current min distance
PossibleMoves SUBROUTINE
        lda #noChoice
        sta GHOST_DIST          ;initialize least distance to a big number
        sta GHOST_DIR           ;initialize best move to an invalid move

        SaveSprite              ;save sprite data into tempvars
.checkup                             ;
        ;; don't reverse
        lda #motionDown
        cmp Sprite_motion,X
        beq .endup
        ;; check if we can go up
        jsr scroll_up           ;
        bcs .endup
        ;; we could go up;
        ldSprtHeadPos Sprite_loc2,W1 ;
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "U",9                ;
.endup        
.checkleft
        ;; don't reverse
        lda #motionRight
        cmp Sprite_motion,X
        beq .endleft
        ;; check if we can go left
        jsr ScrollLeft               ;
        bcs .endleft
        ;; we could go left
        ldSprtHeadPos Sprite_loc2,W1 ;
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "L",5                ;
.endleft        
.checkdown
        ;; don't reverse
        lda #motionUp
        cmp Sprite_motion,X
        beq .enddown
        ;; check if we can go down
        jsr scroll_down         ;
        bcs .enddown
        ;; we could go down
        ldSprtTailPos Sprite_loc2,W1 ;correct
        jsr CalcDistance
        jsr RestoreSprite
        IfFocus "D",13
.enddown        
.checkright
        ;; don't reverse
        lda #motionLeft       
        cmp Sprite_motion,X
        beq .endright
        ;; check if we can go right
        jsr scroll_right            ;
        bcs .endright
        ;; we could go right
        ldSprtTailPos Sprite_loc2,W1 ;correct
        jsr CalcDistance             ;
        jsr RestoreSprite            ;
        IfFocus "R",1                ;
.endright        
.done
        rts
;;; this routine calculates inky's target tile
;;; for either X or Y 
;;; remember inky aims based on both pacman's and blinky's position
;;; see pacman dossier for more details
;;; arguments are:
;;;  BlinkyRow,pacrow +2 tiles,inky target row (output)
;;;  e.g. InkyTargetTile DIV22_RESLT,GHOST1_TGTROW,GHOST_TGTROW
        MAC InkyTargetTile
        lda {1}
        sec
        sbc {2}
        asl             ;DoubleSigned -adding this qty to blinky's position gives the target tile
        sta {3}
        lda {1}
        sec
        sbc {3}
        bpl .notneg
        lda #0
.notneg   
;        clc
;        adc {3}
        sta {3}
        ENDM

;;; ghost running away AI
;;; random turns selected
FrightAI SUBROUTINE
        ;; pick a random offset from pacman's location
        ;; to become our target tile
        store16 screen,W2       ;because ScreentoColRow uses W1
        jsr rand_8
        pha
        jsr rand_8
        ;;  one random number is pushed on the stack
        clc
        adc W2+1
        sta W2+1
        lda #0
        adc W2
        sta W2
        pla
        clc
        adc W2+1
        sta W2+1
        lda #0
        adc W2
        sta W2
        ;; W2 is the screen location of our target tile ;

        ScreenToColRow W2,GHOST_TGTCOL,GHOST_TGTROW
;        Display1 "X",0,GHOST_TGTCOL ;
;        Display1 "Y",3,GHOST_TGTROW
;        Display2 "W",0,W1+1,W1
        
        rts
;;;
;;; scatter / chase are
;;; 7,20
;;; 7,20
;;; 5,20
;;; 5, permanent
;;; on level change after 2
;;; 3d chase mode goes higher
;;; next scatter to 1/60
;;; level 5 scatter goes to 5 seconds
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
;;; the ghost that runs away when pacman is too close
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
        cmp #7
        beq .tooclose
        bcc .tooclose
        ;; not too close, pursue
        lda #PURPLE
        sta Sprite_color,X
        rts
.tooclose
        ;; run away to opposite quadrant of pacman
;        lda #WHITE
;        sta Sprite_color,X
        lda PACCOL
        cmp #11
        bcs .paconright
        ;; pac on left
        lda #22-5
        bne  .testvert          ;jmp
.paconright
        lda #5
.testvert                       ;test vertical quadrants
        sta GHOST_TGTCOL        ;save target column
        lda PACROW              ;determine on top or bottom
        cmp #11
        bcs .paconbottom
        ;; pac on top
        lda #15
        bne .leave
.paconbottom        
        lda #4
.leave        
        sta GHOST_TGTROW
        rts
;;;
;;; ghost 1
;;; draws a vector from his initial target tile ( 2 in front of pacman )
;;; to the current position  of blinky
;;; doubles the length of the vector
;;; and uses the result as his target tile
Ghost1AI SUBROUTINE

        saveX
        ;; our initial target is 2 in front of pacman
        ;; we'll leverage ghost 3's routines for us
        ;; his target tile is 4 in front of pacman and he can
        ;; calc +2 for us
        jsr Ghost3AI

        ;; blinky ( ghost 2 ) calculated just before us, so we'll use
        ;; his position
        ldx #2
        move16x Sprite_loc2,W1
        sub16Im W1,screen ;w1 = offset from screen start, input to divide
        jsr Divide22_16   ;DIV22_RSLT = blinky's row, W1 = col
;        Display1 "Y",2,GHOST1_TGTROW
;        Display1 "X",5,GHOST1_TGTCOL
        
        ;; determine the Y distance between blinky and our target tile
        ;; then , double it
        InkyTargetTile DIV22_RSLT,GHOST1_TGTROW,GHOST_TGTROW
        InkyTargetTile W1,GHOST1_TGTCOL,GHOST_TGTCOL

;        Display1 "Y",2,GHOST_TGTROW
;        Display1 "X",5,GHOST_TGTCOL
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
;        Display1 "Y",2,GHOST_TGTROW
;        Display1 "X",5,GHOST_TGTCOL
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
        sbc #2
        sta GHOST1_TGTCOL
        sbc #2
        sta GHOST_TGTCOL
        rts
.right
        lda PACCOL
        clc
        adc #2
        sta GHOST1_TGTCOL
        adc #2
        sta GHOST_TGTCOL
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
        ldx #4
.loop
        move16x Div22Table,DIV22_WORK
        cmp16 W1,DIV22_WORK
        bcc .1
        sub16 W1,DIV22_WORK
.1
        rol DIV22_RSLT
        dex
        bpl .loop
.done
        pla
        tax

        rts

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
        and #VALIDMOVE
        eor #VALIDMOVE
        beq .uselast
        lda LASTJOY
        jmp .process
.uselast
#ifconst _SLOWPAC        
        rts
#endif        
        ldy MOVEMADE            ;if we made a move already then exit
        beq .cont
        ;;  we must have failed to move?
        jsr EatSoundOff
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
        and #JOYR
        beq .right
        rts
.left
        store16 PAC_L1,W3
        lda #motionLeft
        sta Sprite_motion
        jsr ScrollLeft
        bcs .uselast            ;couldn't move, use last reading
        bcc .moveok
.up
        store16 PAC_UP1,W3
        lda #motionUp
        sta Sprite_motion
        jsr scroll_up
        bcc .moveok
        bcs .uselast
.down
        lda #1
        sta S1                  ;eat dots from tail
        store16 PAC1D,W3
        lda #motionDown
        sta Sprite_motion
        jsr scroll_down
        bcc .moveok
        bcs .uselast
.right
        lda #1
        sta S1                  ;eat dots from tail
        store16 PAC1,W3    ;
        lda #motionRight
        sta Sprite_motion
        jsr scroll_right
        bcc .moveok
        bcs .uselast

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
        cpy GHOST_ROW
        bne .notspecial
        ;; in the box, but at least get to pixel > 4
        lda #4
        cmp Sprite_offset,X
        bcc .notspecial         ;not in the box far enough yet
        ;; we were eaten, and just returned to the box
        lda #modeInBox          ;change mode to inbox
        sta Sprite_mode,X       ;save it
        lda #motionUp           ;set upward, out of box
        sta Sprite_motion,X     ;store it on sprite
        jsr LeaveBox            ;send em right back out
        jmp .notspecial
.special                        ;above the box
        cmp #modeLeaving
        bne .eaten
        lda #modeOutOfBox
        sta Sprite_mode,X
        jmp .notspecial
.eaten                          ;ghost eyes returning to box
        lda #motionDown
        sta Sprite_motion,X
        jsr scroll_down
.notspecial
DeathDistance equ 3
        ;; output: S1 = x pixels, S2 = y pixels
        TileToPixels GHOST_COL,GHOST_ROW,S1,S2
        lda S1                  ;x pixels
        sec
        sbc PACXPIXEL
        Abs
        cmp #DeathDistance
        bcs .done

        lda S2                  ;y pixels
        sec
        sbc PACYPIXEL
        Abs
        cmp #DeathDistance
        bcs .done
        ;; collision between pacman and ghost
        lda Sprite_mode,X
        cmp #modeFright
        beq .ghost_eaten
        cmp #modeOutOfBox       ;are we on the hunt?
        ;;no, then nothing of interest, we were eyes for example
        ;; passing pacman
        bne .done             
        ;; pacman eaten
#ifnconst INVINCIBLE
        jsr death               ;long jumps to level restart
#endif        
        ;; control cannot reach here
        rts                     ;leave this here for debugging and commenting jsr death out
.ghost_eaten                    ;pacman has eaten a ghost in the maze
        lda #modeEaten          ;load eaten mode
        cmp Sprite_mode,X       ;check if already in eaten mode
        beq .done               ;already eaten, skip ahead
        jsr SoundGhostEaten     ;play eaten sound
        sta Sprite_mode,X       ;change sprite mode to eaten
        store16x BIT_EYES,Sprite_src
        lda #eyeSpeed           ;eyes as fast as possible
        sta Sprite_speed,X      ;store eye speed

        saveX
;;; todo: maybe, display ghost eaten score
;        move16x Sprite_loc,W1
;        store16 ready_msg,W2
;        jsr ndPrint
;        WaitTime 3
;        jsr rsPrint
        ldx EatenIdx
        bmi .skip               ;did we run off the points table?
        lda pointTable,x        ;load low byte
        dex                     ;move to high byte
        ldy pointTable,x        ;load high byte
        dex                     ;move to next table entry
        stx EatenIdx            ;save updated index
        jsr UpdateScore         ;A,Y are inputs
        resX                    ;restore X reg
.skip                           ;really shouldn't be here
        ;; we ran off the beginning of the point table
        
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
        cmp #pacframes
        beq .reverse
        sta Sprite_frame
        rts
.reverse
        lda #$ff
        eor PACFRAMED
        ora #1
        sta PACFRAMED
        clc
        adc Sprite_frame
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
;;; composites the tiles underneath a sprite with
;;; the current sprite image
CompositeSprite SUBROUTINE

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
        sta Sprite_sback,X
.checktail
        lda Sprite_sback2,X
        cmp #NOTCOPY            ;is tail tile filled in?
        bne .alldone
        lda (W2),Y              ;nope, fill it with screen
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
        ;; we checked our head and tail against oncoming sprites heads , now
        ;; it's time to check our head n tail against oncoming sprites tails
.not_tail2head        
        ldSprtTailPos Sprite_loc,W3  ;S3 sprite's current tail position into W3
        cpx  SPRT_CUR                ;are we checking against ourselves
        beq .ourselves2              ;yes, then W3 is good to go
        ldSprtTailPos Sprite_loc2,W3 ;not ourselves, load W3 with S3 sprites's new position
.ourselves2        
        cmp16 W1,W3                  ;our head == sprite_idx's tail?
        bne .not_head2tail
        jsr head2tail 
.not_head2tail        
        cmp16 W2,W3                  ;our tail  == sprite_idx's tail?
        bne .not_tail2tail
        jsr tail2tail
.not_tail2tail        
        jmp .loop                    ;all possible collisions checked
        
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
        ;; default ghost speed into A
        ;; used when ghosts are exiting the tunnels
        ;; to restore their speed to what it was when they entered
        MAC RestoreSpeed
        ;; if ghosts are frightened leave their speed alone
        lda Sprite_mode,X
        cmp #modeFright
        beq .done
        lda Sprite_base,X
.done        
        ENDM
;;; set the speed for an individual ghost
;;; X = ghost to set
;;; A = new speed
;;; note: Z != 0 on exit please
SetSpeed subroutine
        sta Sprite_speed,X ;
;        sta Sprite_turn,X
        rts
;;; handle tunnel left side
;;; y must be undamaged on return
DecrementHPos SUBROUTINE
#ifnconst PACTUNNEL        
        cpx #0           ;slowing/speeding only applies to ghosts
        beq .1
#endif
        cmp16Im W2,[tunnelRow*22]+tunnelRCol-tunnelLen+screen
        bne .0
        RestoreSpeed     ;leaving tunnel from right, speed up
        bne .set_speed   ;jmp to set_speed
.0                       ;not exiting the tunnel
        cmp16Im W2,[tunnelRow*22]+tunnelLCol+tunnelLen+screen
        bne .1
        ;; entered from the left
        GetTunnelSpeed  
.set_speed
        jsr SetSpeed
        bne .done               ;jmp .done
.1        
        cmp16Im W2,[tunnelRow*22]+tunnelLCol+screen
        bne .done
        ;; leaving from the left, warping to the right
        store16 [tunnelRow*22]+tunnelRCol+screen,W2
        rts
.done
        Dec16 W2
        rts
;;; handle tunnel right side
IncrementHPos SUBROUTINE
#ifnconst PACTUNNEL        
        cpx #0                ;slowing/speeding only applies to ghosts
        beq .1
#endif        
        cmp16Im W2,[tunnelRow*22]+tunnelLCol+tunnelLen+screen
        bne .0
        RestoreSpeed         ; leaving tunnel from left, speed up
        bne .set_speed
.0        
        cmp16Im W2,[tunnelRow*22]+tunnelRCol-tunnelLen+screen
        bne .1
        GetTunnelSpeed  
.set_speed        
        jsr SetSpeed
        bne .done
.1        
        cmp16Im W2,[tunnelRow*22]+tunnelRCol+screen
        bne .done
        ;; leaving from the right, warping to the left
        store16 [tunnelRow*22]+tunnelLCol+screen,W2
        rts
.done
        Inc16 W2
        rts
        ;; add the scroll value to a sprite
        ;; handles the different speeds
        MAC AddScroll
        clc
        adc SCRL_VAL
        ENDM
        ;; not used currently
        MAC ApplyAllBonus
        tay
        cpx #0
        beq .done1
        lda {1}
        and #1
        bne .done0
.0
        tya
        AddScroll
        jmp .done1
.done0
        tya
.done1        
        ENDM
        ;; extra speed bonus for eyes
        ;; nearly double speed
        ;; 2 pixel increments when not at 0 or 8
        MAC ApplyScroll
        AddScroll
        ldy Sprite_mode,X
        cpy #modeEaten
        bne .done
        cmp END_SCRL_VAL
        beq .done
        AddScroll
.done        
        ENDM
;;; move sprite horizontal
ScrollHoriz SUBROUTINE
        lda Sprite_dir,X        ;get our current orientation
        cmp #dirHoriz           ;are we already horizontal
        beq .ok                 ;ok to move horizontal
        jmp  changehoriz        ;no, switch to horizontal and rts
.ok
        lda Sprite_offset,X     ;get our current pixel offset
        cmp END_SCRL_VAL        ;are we at the end of our tiles?
        bne .draw               ;no, we can smooth scroll
.course                         ;course scroll

        move16x Sprite_loc,W2   ;screen location to W2
        lda SCRL_VAL            ;load scroll direction 
        bmi .left               ;branch to left code
        ;; going right
        lda #0                  ;push potential new sprite offset
        sta NewOffset
        jsr IncrementHPos        ;move over to right one tile
        ldy #01                 ;
        lda (W2),Y              ;check for wall at pos + 2
        jsr IsWall              ;remember we are 2 tiles wide
        bne .continue           ;there is no wall, we can move
.cantmove
        sec                     ;can't move, return false
        rts
.left
        lda #8
        sta NewOffset
        jsr DecrementHPos
        ldy #0
        lda (W2),Y
        jsr IsWall
        beq .cantmove
.continue
        move16x2 W2,Sprite_loc2   ;save the new sprite screen location
 ;       pla                      ;pull new sprite offset from the stack
        lda NewOffset
.draw
        ApplyScroll

        sta Sprite_offset2,X
        clc
        rts
;;; attempt to turn the dot eating sound back on
;;; green border = eating
;;; note the inverse logic on eat_halt, maybe I could
;;; fix this?  probably did it to save a byte somewhere
SoundOn SUBROUTINE
;        lda #13
;        sta 36879
        lda eat_halt            
        bne .done               ;it's already on, nothing to do
        lda #1
        sei
        sta eat_halt
        sta eat_halted
        cli
.done        
        rts
;;; attempt to turn the dot eating sound off
;;; Z true on exit
EatSoundOff SUBROUTINE
        lda Sprite_base
        sta Sprite_speed
        lda eat_halted          ;already halted
        beq .done               ;nothing to do
;        lda #8
;        sta 36879
        
        lda #0
        sei
        sta eat_halt            ;request stop at next cycle
        cli
.done        
        rts
;;; update player scorem chains into DisplayScore routine
;;; value to add is in A (low)
;;; and Y (high)
;;; 10000
UpdateScore subroutine
        sei                     ;prevent 6502 bcd/interrupt 'bug'
        sed                     ;decimal mode
        clc
        adc PlayerScore_l
        sta PlayerScore_l
        tya
        adc PlayerScore_m
        sta PlayerScore_m
        bcc .0
        lda PlayerScore_h
        adc #0
        sta PlayerScore_h
.0
        cld                     ;clear dec mode
        cli
DisplayScore subroutine        
        ldy #5                  ;six score digits
.loop
        tya
        lsr
        tax
        lda PlayerScore_h,X
        jsr SplitByte            ;A=high nibble on exit, S1=low
        clc
        adc #48 | $80
        sta screen+20-6,y
        txa                     ;high nibble
        adc #48 | $80
        dey
        sta screen+20-6,y
        dey
        bpl .loop
        ;; check if bonus life
        lda BonusAwarded
        bne .done
        lda PlayerScore_h
        cmp #BONUSLIFE
        bcc .done
        jsr IncrementLives
.done        
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
        lda #1                  ; offset to blank tile in endtile case
        sta S3                  ;-offset to ORG in endtile case
        lda Sprite_offset,X     ;where is sprite offset
        cmp CORNER_SHAVE        ;within shave dist of beg tile?
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
        
        lda #1                  ;change orientation to horiz
        sta Sprite_dir2,X


        lda SPRT_LOCATOR        ;moving right?
        cmp #motionRight
        beq .isright            ;branch moving right
.isleft
        lda #7                  ;going left, sprite offset = 8
;        sec
;        sbc CORNER_SHAVE        ;bonus for pacman if applic
        sta Sprite_offset2,X

        lda #22                 ;offset from ORG for new sprite pos
        bne .continue           ;jmp .continue
.isright
        lda #1                  ;assume moving right
;        clc
;        adc CORNER_SHAVE        ;bonus for pacman if applic
        sta Sprite_offset2,X     ;sprite offset = 0

        lda #23                 ;offset from ORG for new sprite pos
.continue        
        sta S3                  
        addxx W2,W1,S3          ;update new sprite pos
        move16x2 W1,Sprite_loc2

        clc                     ;return success
        rts
        ;; 
        MAC CheckGhostBox
        lda W2
        clc
        adc SPRT_LOCATOR
        sta W1
        lda W2+1
        adc #0
        sta W1+1
        cmp16Im W1,ghostBoxHall
        bne .done               ;not ghost box, ok to proceed
        
        lda Sprite_mode,X       ;what mode is sprite in
        cmp #modeOutOfBox       ;are we normal out of box mode?
        beq .done               ;yes, then its considered a wall
        cmp #modeFright         ;are we frightened out of box mode?
        beq .done               ;yes, then its considered a wall
        cmp #modePacman         ;and pacman ain't allowed in here
        beq .done
        cmp #modeFright0
        beq .done
        ;; we are eaten or leaving the box, this move is ok
        ;note: Z=0
.done        
        ENDM
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
        ldy #22                 ;
        sty S3                  ;offset to ORG in endtile case
        lda Sprite_offset,X     ; where is sprite offset
        cmp CORNER_SHAVE        ; within shave dist of 0?
        bcc .begtile
        beq .begtile            ; at beginning of tile
        clc
        adc CORNER_SHAVE        ; are we within shave distance
        cmp #8                  ; of end of tile?
        bcs .endtile
ChangeVertFailed       ; we can't change directions  in middle of tile
        sec
        rts
.begtile                        ;at beginning of tile
        ldy #23
        sty S3                  ;update ORG offset
.endtile                        ;end of tiles branch
        subxx W1,W2,S3          ;generate ORG address in W2

        CheckGhostBox
        beq ChangeVertFailed
        
        ldy SPRT_LOCATOR
        lda (W2),Y
        jsr IsWall
        beq ChangeVertFailed    ;we hit a wall, abort move
        
        lda #dirVert            ;change orientation to vertical
        sta Sprite_dir2,X

        lda SPRT_LOCATOR        ;moving down?
        cmp #motionDown
        beq .isdown             ;branch down
.isup
        lda #7                  ;going up
;        sec
;        sbc CORNER_SHAVE         ;bonus for pacman if applicable
        sta Sprite_offset2,X    ;this would be new sprite offset

        lda #1                  ;offset from ORG for new sprite pos
        bne .continue           ;jmp .continue
.isdown
        lda #1                  ;going down
;        clc
;        adc CORNER_SHAVE        ;bonus for pacman if applicable
        sta Sprite_offset2,X    ;this would be new sprite offset

        lda #23                 ;offset from ORG for new sprite pos
.continue        
        sta S3                  
        addxx W2,W1,S3          ;update new sprite pos
        move16x2 W1,Sprite_loc2
        clc                     ;return success
        rts
        ;; move sprite_loc -22 into {1}
        ;; this is the vertical 'reference' tile
        MAC SpriteVRef
        saveX
        txa
        asl
        tax
        lda Sprite_loc,X
        sec
        sbc #22
        sta {1}
        inx
        lda Sprite_loc,X
        sbc #0
        sta {1}+1
        resX
        ENDM
        
ScrollVertical SUBROUTINE
        lda Sprite_dir,X
        cmp #dirVert            ;check if already vertical
        beq .00

        jmp changevert          ;if not, change us to down and rts
.00
        lda Sprite_offset,X
        cmp END_SCRL_VAL        ; less than 8? then fine scroll
        bne .fine
.course                         ; course scroll
        SpriteVRef W2           ; VREF tile loc-22
        lda SCRL_VAL
        bmi .up
        ;; going down
        ldy #66                 ;check one tile down from tail
        lda #0                  ;push new sprite offset
        pha
        beq .wallcheck          ;jmp .wallcheck
.up
        lda #8                  ;push new sprite offset
        pha
        ldy #0                  ;check one tile above head
.wallcheck
        sty S1
        lda (W2),Y              ;offset from VREF tile
        jsr IsWall              ;is there a wall character there?
        bne .continue
        pla
        sec                     ;indicate failure
        rts
.continue
        lda S1                  ;if was up, no update of W2 needed
        beq .wasup
        add16Im W2,44           ;VREF+44 is new screen loc
.wasup        
        move16x2 W2,Sprite_loc2  ;save the new sprite screen location
        pla
.fine
        ApplyScroll
        sta Sprite_offset2,X
.done
        clc
        rts
;;; 
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

;;; make letter code for display
mkletter eqm [..-"A"+1|$80]
ready_msg        
        dv.b mkletter "R","E","A","D","Y"
ready_msg_sz equ * - ready_msg   ; length of msg
        dc.b 0
gameover_msg
        ;; text of game over message
        dv.b mkletter "G","A","M","E"
        dc.b EMPTY
        dv.b mkletter "O","V","E","R"
gameover_msg_sz equ * - gameover_msg ; length of msg
        dc.b 0
#ifconst LARGEMEM
panicman_msg
        dv.b mkletter "P","A","N","I","C","M","A","N"
        dc.b 0
jmessner_msg
        dv.b mkletter "J"
        dc.b EMPTY
        dv.b mkletter "M","E","S","S","N","E","R"
        dc.b 0
dbills_msg
        dv.b mkletter "D"
        dc.b EMPTY
        dv.b mkletter "B","I","L","L","S"
        dc.b 0
#endif        
;;; 
;;; non destructive print
;;; save text to SaveBuffer
;;; W1=screen loc
;;; W2=text to print
;;; Y = length
ndPrint subroutine
        ldy #0
.0
        lda (W1),Y              ;save char underneath
        sta SaveBuffer,Y
        lda (W2),y              ;load text to print
        beq .done
        sta (W1),Y
        iny
        bne .0                  ;jmp .0
.done
        sta SaveBuffer,Y
        clc
        lda W1+1
        pha                     ;save W1
        adc #clroffset
        sta W1+1
        lda #WHITE
        dey
.clrloop
        sta (W1),Y
        dey
        bpl .clrloop
        pla
        sta W1+1                ;restore W1
        rts
;;; restore text underneath a previous
;;; ndPrint call
rsPrint subroutine
        ldy #0
.1
        lda SaveBuffer,y
        beq .done
        sta (W1),Y
        iny
        bpl .1
.done        
        rts
        
#ifconst LARGEMEM
;;;
;;;  copy the stock character set
;;;
copychar    SUBROUTINE
        store16 chrom1,W2           ;source
        store16 [mychars+$400],W3     ;dest
        ldy #0
.loop
        lda (W2),y
        sta (W3),y
        inc16 W2
        inc16 W3
        cmp16Im W2,[chrom1+$200]
        beq .done
        jmp .loop
.done
        rts

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

#endif                          ;LARGEMEM
        
#if 0

TEXTS        
        dv.b mkletter "C","O","D","E"

        dc.b 58|$80
        dv.b mkletter "D","A","N","E"
        dv.b mkletter "M","U","S","I","C"
        dc.b 58|$80
        dv.b mkletter "J","E","F","F"

TEXTE        
;;; message box in center of screen
splash subroutine


        store16 screen+22*4,W1
;        add16Im W1,22
;        jsr print
        store16 ready_msg,W2
        jsr getReady
        
        ldx #21
.loop        
        lda #HWALL
        sta screen+[tunnelRow-1]*22,x
        sta screen+[tunnelRow+1]*22,x

        dex
        beq .done
        bne .loop

.done        
        rts
#endif        
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
#if 0        
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
#endif        

;;;
;;;
;;;
#ifnconst LARGEMEM        
        org $1c00-(8*3)
        INCLUDE "bitmaps.asm"
#endif
        
#if 0
        
for debugging, here are the keyboard read codes for the ghost control
up 9,down 26,left 17,right 18 a,d w,x
        
tile scrolling works
we allow going to 8 or 0 which always allows to make it to the end of a tile set
so if you are heading towards a wall, you will always be able to touch it
when you start in the new tile after a course scroll it's always 1 or 7         

        ghost box
        row 11
        col 9
        
        Speed denominators
        
        .40 | 120
        .45 | 110
        .50 | 100
        .55 | 90
        .60 | 80
        .71 | 58
        .79 | 42
        .80 | 40
        .83 | 37
        .85 | 30
        .87 | 26
        .90 | 20
        .95 | 10
        
Arcade settings for pacman speed
        
level| norm | dots | power | pwrdot
-----------------------------------
1      80%    71%    90%     79%   
2-4    90%    79%    95%     83%   
5-20   100%   87%    100%    87%   
21+    90%    79%     -

Arcade settings for ghost speed

level | normal | power | tunnel
-----------------------------------
1       75%      50%     40%
2-4     85%      55%     45%
5-20    95%      60%     50%
21+     95%       -      50%

        napkin calculation for above percentages:
        60 FPS * .71 = 42.6 present
        * 10 to get rid of floating point
        426/600 present = 142/200 present
        200-142 = 58 skipped
        200/58 = fraction = 3.44
        We'll use integer addition with error correction
        add 58, each time we > 200 , the emit an error signal ( skip a frame )
        and take N-200 to generate the next number in the sequence
        e.g. the sequence 58,116,174,32,90,148, 6,64,122,180
                                      X         X
        you can see the sequence varies between runs of 2 and 3 continuous
        frames, which does net out 58 skipped frames over 200 as we desire

        in the table above I've put the denominator for each speed
        
11/6/17 ghost should not reverse direction right away when the mode changes, but rather when they enter the next tile
        eating an energizer stop pacman moving for three frames
        ghosts leaving the house direction does not match the arcade game

#endif
