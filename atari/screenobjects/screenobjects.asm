    processor 6502

    include "vcs.h"
    include "macro.h"

;;;;;;;;;
; start segment for variable declaration
;;;;;;;;;;;;;;;;;;

    seg.u Variables
    org $80
P0Height ds 1 ; defines space of 1 byte for play 0 height
P1Height ds 1


    seg Code
    org $F000

Reset:
    CLEAN_START

    ldx #$80 ; blue
    stx COLUBK

    lda #%1111 ; white
    sta COLUPF

    lda #10
    sta P0Height
    sta P1Height

;;;;;;;;;;;;;;;;;;;;;;
;; load player colors
;;;;;;;;;;;;;;;;;;;;;;

    lda #$48 ; red
    sta COLUP0

    lda #$C6 ; green
    sta COLUP1

    ldy #%00000010 ; CTRLPF D1 set to 1 means score
    sty CTRLPF

;;;;;;;;;;;;;;;;;;;;;
;; start rendering frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:
    lda #2
    sta VBLANK
    sta VSYNC

    ;;;;;;;; VSYNC
    REPEAT 3
        sta WSYNC   ; three scanlines for VSYNC
    REPEND
    lda #0
    sta VSYNC

    ;;;;;;;;;;;;;;VBLANK
    REPEAT 37
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

VisibleScanlines:
    ; draw 10 empty scanlines
    REPEAT 10
        sta WSYNC
    REPEND

    ;;;;;;;;;;;;;
    ;;; dislay scoreboard
    ;;;;;;;;;;;;;;;;;;;;
    ldy #0
ScoreboardLoop:
    lda NumberBitmap,Y
    sta PF1
    sta WSYNC
    iny
    cpy #10
    bne ScoreboardLoop

    lda #0
    sta PF1

    ; draw 50 empty scanlines

    REPEAT 50
        sta WSYNC
    REPEND

    ;;;;;;;;;;;;
    ;;PLayer graphics
    ;;;;;;;;;;;;;;;;;;

    ldy #0
Player0Loop:
    lda PlayerBitmap,Y
    sta GRP0
    sta WSYNC
    iny
    cpy P0Height
    bne Player0Loop

    lda #0
    sta GRP0

    ldy #0
Player1Loop:
    lda PlayerBitmap,Y
    sta GRP1
    sta WSYNC
    iny
    cpy P1Height
    bne Player1Loop

    lda #0
    sta GRP1

    ;;;;;;;;;;
    ;;draw remaining scanlines
    ;;;;;;;;;;;;;;;;;;;;;;;;

    REPEAT 102
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;
;; VBLANK
;;;;;;;;;;;;;;;;;;;;;

    REPEAT 30
        sta WSYNC
    REPEND

    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the last ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFE8
PlayerBitmap:
    .byte #%01111110   ;  ######
    .byte #%11111111   ; ########
    .byte #%10011001   ; #  ##  #
    .byte #%11111111   ; ########
    .byte #%11111111   ; ########
    .byte #%11111111   ; ########
    .byte #%10111101   ; # #### #
    .byte #%11000011   ; ##    ##
    .byte #%11111111   ; ########
    .byte #%01111110   ;  ######

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defines an array of bytes to draw the scoreboard number.
;; We add these bytes in the final ROM addresses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFF2
NumberBitmap:
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########
    .byte #%00000010   ;      ###
    .byte #%00000010   ;      ###
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########
    .byte #%00001000   ; ###
    .byte #%00001000   ; ###
    .byte #%00001110   ; ########
    .byte #%00001110   ; ########

    org $FFFC
    .word Reset
    .word Reset
