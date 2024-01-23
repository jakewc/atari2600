    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare variables from memory pos $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte        ; player 0 x-position
JetYPos         byte        ; player0 y-position
BomberXPos      byte        ; player1 x-position
BomberYPos      byte        ; player1 y-position
MissileXPos     byte
MissileYPos     byte
Score           byte        ; Score stored as BCD
Timer           byte        ; Timer stored as BCD
Temp            byte        ; auxilliary variable to store temporary score calculation vars
OnesDigitOffset word        ; lookup table offset for score 1's digit
TensDigitOffset word        ; lookup table offset for score 10's digit
JetSpritePtr    word        ; pointer to player 0 sprite lookup table
JetColorPtr     word        ; pointer to player 0 color
BomberSpritePtr word        ; player 1 sprite lookup
BomberColorPtr  word        ; player 1 color
JetAnimOffset   byte        ; player0 sprite frame offset for animation
Random          byte        ; random number generated to set enemy position
ScoreSprite     byte        ; sprite for the score
TimerSprite     byte        ; sprite for the timer
TerrainColor    byte        ; current terrain color
RiverColor      byte        ; current river color

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9              ; P0 sprite height
BOMBER_HEIGHT = 9           ; P1 sprite height
DIGITS_HEIGHT = 5           ; scoreboard didgit height

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START         ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers (background colors, positions etc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta JetXPos         ; JetXPos = 68
    lda #10
    sta JetYPos         ; JetYPos = 10
    lda #62
    sta BomberXPos
    lda #83
    sta BomberYPos
    lda #%11010100
    sta Random          ; Random = $D4
    lda #0
    sta Score
    sta Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%000000000
        cpx MissileYPos     ; compare current scanline (x) with current missile Y pos
        bne .SkipMissileDraw
.DrawMissile
        inc MissileYPos         ; MissileYPos++
        lda #%00000010          ; enable missile 0 display
.SkipMissileDraw:               ; pass
        sta ENAM0               ; store the correct value in the missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr        ; lo-byte ptr for jet sprite lookup table
    lda #>JetSprite         ; hi-byte
    sta JetSpritePtr+1

    lda #<JetColor
    sta JetColorPtr         ; lo-byte ptr for jet sprite lookup table
    lda #>JetColor          ; hi-byte
    sta JetColorPtr+1

    lda #<BomberSprite
    sta BomberSpritePtr     ; lo-byte ptr for jet sprite lookup table
    lda #>BomberSprite      ; hi-byte
    sta BomberSpritePtr+1

    lda #<BomberColor
    sta BomberColorPtr      ; lo-byte ptr for jet sprite lookup table
    lda #>BomberColor       ; hi-byte
    sta BomberColorPtr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Display VSYNC & VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC
    sta VBLANK          ; turn these on
    REPEAT 3
        sta WSYNC       ; display 3 VSYNC lines
    REPEND
    lda #0
    sta VSYNC           ; turn off vsync
    REPEAT 31
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the vblank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos       ; set player 0 horizontal position

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos       ; set player 1 horizontal position

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos

    jsr CalculateDigitOffset ; scoreboard digit lookup table offset

    jsr GenerateJetSound     ; enable jet engine sound

    sta WSYNC
    sta HMOVE               ; apply horizontal offsets previously set by routines

    lda #0
    sta VBLANK              ; turn off vblank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 20 scoreboard scanlines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta COLUBK
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF

    lda #$1E
    sta COLUPF          ; set scoreboard playfield color to yellow

    ldx #DIGITS_HEIGHT

.ScoreDigitLoop:
    ldy TensDigitOffset ; get the tens digit offset for the score
    lda Digits,Y        ; getting the right digit by adding the offset
    and #$F0            ; mask ones digit
    sta ScoreSprite     ; save tens in a variable

    ldy OnesDigitOffset ; ones digit offset
    lda Digits,Y
    and #$0F            ; mask tens digit
    ora ScoreSprite     ; merge the two digits
    sta ScoreSprite
    sta WSYNC
    sta PF1             ; update playfield to display scoreboard

    ldy TensDigitOffset+1   ; get the tens digit offset for the score
    lda Digits,Y            ; getting the right digit by adding the offset
    and #$F0                ; mask ones digit
    sta TimerSprite         ; save tens in a variable

    ldy OnesDigitOffset+1     ; ones digit offset
    lda Digits,Y
    and #$0F                ; mask tens digit
    ora TimerSprite         ; merge the two digits
    sta TimerSprite

    jsr Sleep12Cycles       ; waste 12 cycles

    sta PF1                 ; update playfield to display timer

    ldy ScoreSprite         ; preload for the next scanline
    sta WSYNC

    sty PF1                 ; update playfield for the score display
    inc TensDigitOffset     ; inc all digits for next line of data
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles

    dex
    sta PF1
    bne .ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 85 Visible scanlines (because 2-line kernel - 2 per loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLines:
    lda TerrainColor           ; green grass
    sta COLUPF

    lda RiverColor            ; blue background
    sta COLUBK

    lda #%00000001
    sta CTRLPF          ; reflect playfield reflection
    lda #$F0
    sta PF0             ; setting PF0 bit pattern
    lda #$FC
    sta PF1             ; PF1 bit patterns
    lda #0
    sta PF2             ; PF2 bit pattern

    ldx #89             ; X counts the number of remaining scanlines
.GameLineLoop:
    DRAW_MISSILE            ; draw a missile using a macro
.IsInsideJetSprite:
    txa                     ; transfer x to A
    sec                     ; set carry flag before subtraction
    sbc JetYPos             ; subtract jet y pos from X
    cmp #JET_HEIGHT          ; are we inside jet sprite
    bcc .DrawSpriteP0
    lda #0                  ; else set lookup index to 0
.DrawSpriteP0:
    clc                     ; clear carry
    adc JetAnimOffset       ; jump to correct sprite frame address
    tay                     ; load Y so we can work with the pointer
    lda (JetSpritePtr),Y    ; load the player 0 bitmap data from lookup table
    sta WSYNC               ; wait for scanline
    sta GRP0                ; render player 0
    lda (JetColorPtr),Y     ; load player 0 color bitmap data from table
    sta COLUP0              ; set color of player 0

.IsInsideBomberSprite:
    txa                     ; transfer x to A
    sec                     ; set carry flag before subtraction
    sbc BomberYPos          ; subtract bomber y pos from X
    cmp #BOMBER_HEIGHT       ; are we inside bomber sprite
    bcc .DrawSpriteP1
    lda #0                  ; else set lookup index to 0
.DrawSpriteP1:
    tay                     ; load Y so we can work with the pointer

    lda #%00000101
    sta NUSIZ1              ; stretch player 1

    lda (BomberSpritePtr),Y ; load the player 1 bitmap data from lookup table
    sta WSYNC               ; wait for scanline
    sta GRP1                ; render player 1
    lda (BomberColorPtr),Y  ; load player 1 color bitmap data from table
    sta COLUP1              ; set color of player 1

    dex
    bne .GameLineLoop

    lda #0
    sta JetAnimOffset       ; reset Jet animation frame

    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player 0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000      ; joystick up
    bit SWCHA           ; check input with contents of A
    bne CheckP0Down     ; skip, test other input directions
    lda JetYPos
    cmp #70
    bpl CheckP0Down
    inc JetYPos         ; otherwise, move up
    lda #0
    sta JetAnimOffset   ; reset sprite frame

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    lda JetYPos
    cmp #5
    bmi CheckP0Left
    dec JetYPos
    lda #0
    sta JetAnimOffset   ; reset sprite frame

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    lda JetXPos
    cmp #35
    bmi CheckP0Right
    dec JetXPos
    lda #JET_HEIGHT      ; 9
    sta JetAnimOffset   ;set anim offset to second frame

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckButtonPressed
    lda JetXPos
    cmp #100
    bpl CheckButtonPressed
    inc JetXPos
    lda #JET_HEIGHT      ; 9
    sta JetAnimOffset   ;set anim offset to second frame

CheckButtonPressed:
    lda #%10000000      ; if button is pressed
    bit INPT4
    bne Pass
.SetMissilePos:
    lda JetXPos
    clc
    adc #5
    sta MissileXPos     ; missile position == player position
    lda JetYPos
    clc
    adc #8
    sta MissileYPos

    ; fallback when no input was performed
Pass:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caluculations to update positions for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0                      ; cmp bomber Y with 0
    bmi .ResetBomberPosition    ; if less than zero, reset bomber to top
    dec BomberYPos              ; else, decrement to move down
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos      ; call subroutine for next random enemy x pos

.SetScoreValues:
    sed                         ; set decimal mode
    lda Timer
    clc
    adc #1
    sta Timer
    cld                         ; turn off decimal mode

EndPositionUpdate:     ; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for collisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000              ; CXPPMM bit 7 detects p0 and p1 collisions
    bit CXPPMM
    bne .CollisionP0P1
    jsr SetTerrainRiverColor
    jmp CheckCollisionM0P1
.CollisionP0P1:
    jsr GameOver                ; call GameOver subroutine

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P
    bne .M0P1Collision
    jmp EndCollisionCheck
.M0P1Collision:
    sed
    lda Score
    clc
    adc #1
    sta Score                   ; increment score
    cld
    lda #0
    sta MissileYPos             ; reset missile position

EndCollisionCheck:              ; fallback
    sta CXCLR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to StartFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate jet engine sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateJetSound subroutine
    lda #3
    sta AUDV0

    ; divide y position of jet by 8
    lda JetYPos
    lsr
    lsr
    lsr
    sta Temp
    lda #31
    sec
    sbc Temp
    sta AUDF0
    lda #8
    sta AUDC0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reset color defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor
    lda #$84
    sta RiverColor
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal position subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A = target x-coord position in pixels of object
;; Y = object type (0: player 0, 1:player 1, 2: missile0, 3: missile 1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC               ; new scanline
    sec                     ; set flag
.Div15Loop
    sbc #15                 ; subtract 15 from A
    bcs .Div15Loop          ; loop until carry clear
    eor #7                  ; figure out fine offset
    asl
    asl
    asl
    asl                     ; get top 4 bits of offset
    sta HMP0,Y              ; store fine offfset
    sta RESP0,Y             ; fix fine offset
    rts                     ; return subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game over
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor
    sta RiverColor
    lda #0
    sta Score
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate Linear-Feedback shift register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate random number
;; Divide by 4 to limit the size of the result
;; Add 30 to push the result right on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random      ; generate random number
    lsr
    lsr             ; divide value by 4 by performing two right shifts
    sta BomberXPos  ; save it to the variable BomberXPos
    lda #30
    adc BomberXPos
    sta BomberXPos

    lda #96
    sta BomberYPos  ; sets y-position to top of screen

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                  ; loop counter
.PrepareScoreLoop           ; loop twice
    ; loop for ones digit

    lda Score,X             ; load A with Timer (X=1) or Score (X=0)
    and #$0F                ; remove tens digit by logical AND
    sta Temp                ; save the tens value into Temp variable
    asl                     ; shift left (n*2)
    asl                     ; shift left (N*4)
    adc Temp                ; add to itself (N*5)
    sta OnesDigitOffset,X   ; save A in OnesDigitOffset+1 or OnesDigitOffset (which is why the variable is a word)

    ; for tens digit
    lda Score,X
    and #$F0                ; mask the ones digit
    lsr                     ; shift right (N/2)
    lsr                     ; (N/4)
    sta Temp
    lsr                     ; (N/8)
    lsr                     ; (N/16)
    adc Temp                ; add N/4 to N/16
    sta TensDigitOffset,X   ; store A in tens+1 (1111111100000000) or Tens (0000000011111111)

    dex
    bpl .PrepareScoreLoop

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sleep 12 cycles
;; jsr = 6 cycles, rts = 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fill ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset
