    processor 6502

    include "vcs.h"
    include "macro.h"

    seg
    org $F000

Reset:
    CLEAN_START

    ldx #$80
    stx COLUBK

    lda #$1C
    sta COLUPF

StartFrame:
    lda #02
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

    ;;;;;; Set CTRLPF register to reflect playfield
    ldx #%00000001 ;CTRLPF register (D0 is reflect)
    stx CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;
;;write scanlines
;;;;;;;;;;;;;;;;;;;;;;;

    ; skip 7 scanlines with no playfield
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Set top border

    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ;;; set side borders
    ldx #%01100000
    stx PF0
    ldx #0
    stx PF1
    ldx #%10000000
    stx PF2
    REPEAT 164
        sta WSYNC
    REPEND

    ;; bottom border
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ;; bottom empty
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND


;;;;;;;;;;;;;;;;;;;;
;; VBLANK
;;;;;;;;;;;;;;;;;;;;;

    lda #2
    sta VBLANK
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

    jmp StartFrame

    org $FFFC
    .word Reset
    .word Reset
