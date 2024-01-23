    processor 6502

    ;;; includes

    include "vcs.h"
    include "macro.h"

    ;;; start a segment for variable initialization
    ;;; we have $80 to $FF to work with

    seg.u Variables
    org $80
P0XPos  byte    ; sprite X coord

    ;;; Start ROM code segment

    seg Code
    org $F000

Reset:
    CLEAN_START

    ldx #$80    ; blue background
    stx COLUBK

    ldx #$D0
    stx COLUPF  ; grey floor

    ;========================
    ;;; Initialize variables
    ;=========================

    lda #10
    sta P0XPos

    ;;; Start frame

StartFrame:
    lda #2
    sta VBLANK     ; turn VBLANK on
    sta VSYNC   ; turn vsync on


    ;;; 3 lines of vsync

    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

    ;;; Set player X position while in VBlank

    lda P0XPos  ; load A with desired X pos
    and #$7F    ; AND the position with $7F to find the fine range
    sta WSYNC   ; wait for next scanline
    sta HMCLR   ; clear old horizontal position values

    sec         ; set carry flag before subtraction
DivideLoop:
    sbc #15     ;A -= 15
    bcs DivideLoop

    eor #7  ; adjust the reaminder of A between -8 and 7
    asl     ; shift left four times to get most siginifcant bits for HMP0
    asl
    asl
    asl
    sta HMP0    ; set fine position
    sta RESP0   ; reset 15-point fine position
    sta WSYNC   ; wait for next scanline
    sta HMOVE   ; apply the fine position offset
    ;;; 37 lines of vblank

    ;;; Output VBLANK

    REPEAT 35
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

    ;======================
    ;;; Draw visible scanlines
    ;===========================

    REPEAT 160
        sta WSYNC
    REPEND

    ldy #17  ; draw 17 lines of bitmap
DrawBitmap:
    lda P0Bitmap,Y ; load player bitmap slice of data
    sta GRP0    ; set graphics for player 0

    lda P0Color,Y   ; load player color
    sta COLUP0      ; set player 0 color

    sta WSYNC

    dey
    bne DrawBitmap  ; repeat until finished

    lda #0
    sta GRP0    ; disable P0 graphics

    lda #$FF      ;grass playfield
    sta PF0
    sta PF1
    sta PF2

    REPEAT 15
        sta WSYNC
    REPEND

    lda #0      ; disable grass playfield
    sta PF0
    sta PF1
    sta PF2


    ;;; Overscan

Overscan:
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick input test for P0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000
    bit SWCHA
    bne CheckP0Down
    inc P0XPos

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    dec P0XPos

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    dec P0XPos

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput
    inc P0XPos

NoInput:
    ; fallback when no input was performed

    jmp StartFrame



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Bitmap:
    byte #%00000000
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00010100
    byte #%00011100
    byte #%01011101
    byte #%01011101
    byte #%01011101
    byte #%01011101
    byte #%01111111
    byte #%00111110
    byte #%00010000
    byte #%00011100
    byte #%00011100
    byte #%00011100

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    byte #$00
    byte #$F6
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$F2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$C2
    byte #$3E
    byte #$3E
    byte #$3E
    byte #$24

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset
