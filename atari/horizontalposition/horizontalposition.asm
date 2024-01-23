    processor 6502

    include "vcs.h"
    include "macro.h"

    seg.u Variables
    org $80
P0XPos   byte


    seg Code
    org $F000

Reset:
    CLEAN_START

    ldx #$00
    stx COLUBK

    lda #40
    sta P0XPos

StartFrame:
    lda #2
    sta VBLANK     ; turn VBLANK on
    sta VSYNC

    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

    lda P0XPos
    and #$7F

    sta WSYNC
    sta HMCLR

    sec
DivideLoop:
    sbc #15
    bcs DivideLoop

    eor #7
    asl
    asl
    asl
    asl
    sta HMP0
    sta RESP0
    sta WSYNC
    sta HMOVE

    REPEAT 35
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

    REPEAT 60
        sta WSYNC
    REPEND

    ldy 8
DrawBitmap:
    lda P0Bitmap,Y
    sta GRP0

    lda P0Color,Y
    sta COLUP0
    sta WSYNC
    dey
    bne DrawBitmap

    lda #0
    sta GRP0

    REPEAT 124
        sta WSYNC
    REPEND

Overscan:
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Position while between 40th and 80th pixel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda P0XPos
    cmp #80
    bpl ResetXPos
    jmp IncrmXPos
ResetXPos:
    lda #40
    sta P0XPos
IncrmXPos:
    inc P0XPos

    jmp StartFrame

P0Bitmap:
    byte #%00000000
    byte #%00101000
    byte #%01110100
    byte #%11111010
    byte #%11111010
    byte #%11111010
    byte #%11111110
    byte #%01101100
    byte #%00110000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    byte #$00
    byte #$40
    byte #$40
    byte #$40
    byte #$40
    byte #$42
    byte #$42
    byte #$44
    byte #$D2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size adding reset addresses at $FFFC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset
