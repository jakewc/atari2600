    processor 6502

    include "vcs.h"
    include "macro.h"

    seg code
    org $f000   ; defines origin of the ROM

START:
    ;CLEAN_START ; macro to safely clear all memory registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set background luminosity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #$1E    ;load color into register A ($1E is NTSC yellow)
    sta COLUBK  ; store A to Background color address $09 in alias

    jmp START  ; repeat from start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill ROM size to 4kb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word START    ; reset vector
    .word START    ; Interrupt vector
