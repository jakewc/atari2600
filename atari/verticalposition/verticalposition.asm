    processor 6502

    include "vcs.h"
    include "macro.h"

    seg.u Variables
    org $80
P0Height   byte
PlayerYPos byte


    seg Code
    org $F000

Reset:
    CLEAN_START

    ldx #$00
    stx COLUBK

    lda #180
    sta PlayerYPos

    lda #9
    sta P0Height

StartFrame:
    lda #2
    sta VBLANK     ; turn VBLANK on
    sta VSYNC

    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

    REPEAT 37
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK


    ldx #192
Scanline:
    txa
    sec
    sbc PlayerYPos
    cmp P0Height
    bcc LoadBitmap
    lda #0

LoadBitmap:
    tay
    lda P0Bitmap,Y

    sta WSYNC

    sta GRP0

    lda P0Color,Y

    sta COLUP0

    dex
    bne Scanline

Overscan:
    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND


    dec PlayerYPos

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
